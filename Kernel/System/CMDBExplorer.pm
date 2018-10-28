# --
# Copyright (C) 2018- Perl-Services.de, http://perl-services.de
# Copyright (C) 2014-2018 Belnet, http://www.belnet.be/
# Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-CMDBExplorer
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.

# --

package Kernel::System::CMDBExplorer;

=head1 NAME

Kernel::System::CMDBExplorer - trace links between services, config items or FAQs

=head1 SYNOPSIS

This class abstracts a set of ITSM services, config items, FAQs and the links
between them as a "trace" that can be rendered textually (built-in) or 
graphically by C<Kernel::System::CMDBExplorer::GraphVizRenderer>, using the
free graphviz library.

Its purpose is to support visualization of service dependencies either 
directly in the (web-)GUI or from the commandline.

=cut

use strict;
use warnings;

our @ObjectDependencies = qw(
    Kernel::System::Service
    Kernel::System::ITSMConfigItem
    Kernel::System::LinkObject
    Kernel::System::GeneralCatalog

    Kernel::System::CMDBExplorer::ObjectWrapper
    Kernel::System::CMDBExplorer::Scope
    Kernel::System::CMDBExplorer::GraphVizRenderer
);

=head1 PUBLIC INTERFACE

=over 4

=cut

=item new()

Creates an object.

    my $CMDBExplorerObject = $Kernel::OM->Get('Kernel::System::CMDBExplorer');

=cut

sub new {
    my ( $ClassOrType, %Param ) = @_;

    # allocate new hash for object
    my $Self = {};
    my $Class = ref $ClassOrType || $ClassOrType; 
    bless( $Self, $Class );

    my $ConfigObject = $Kernel::OM->Get('Kernel::Config');

    # Get debug setting through config, overriden by parameter
    $Self->{Debug} = $ConfigObject->{'CMDBExplorer::Debug'} || 0;
    $Self->{Debug} = $Param{Debug} if defined $Param{Debug};

    return $Self;
} 

=item GetKnownLinkTypes()

Gets I<all> known link types that may exist between different objects.
For the purpose of link tracing and visualization, the hierarchical 
decomposition of services is modelled as the pseudo link type 
C<ComposedOf>.

Link types are returned as the keys of a HASH ref for usage as lookup 
table, while the values indicate if it is a non-directional link ('=') 
or directed link, with '>' for a "normal" directed link, '!' for I<the> 
dependency link used for incident state propagation, or '+' for the 
pseudo decomposition link.

    my $HashRef = $TraceObject->GetKnownLinkTypes();

=cut

sub GetKnownLinkTypes {
    my ($Self, %Param) = @_;

    # Check cache
    return $Self->{KnownLinkTypes} if $Self->{KnownLinkTypes};

    my $LinkObject   = $Kernel::OM->Get('Kernel::System::LinkObject');
    my $ConfigObject = $Kernel::OM->Get('Kernel::Config');

    # Get Config
    my %TypeList          = $LinkObject->TypeList( UserID => 1 );
    my $IncidentLinkTypes = $ConfigObject->Get('ITSM::Core::IncidentLinkTypeDirection');

    # Create the hash of known link types
    my %KnownLinkTypes = ( ComposedOf => '+' );    # pseudo, for svc breakdown
    for ( keys %TypeList ) {
        if ( $IncidentLinkTypes->{$_} ) {
            $KnownLinkTypes{$_} = '!';
        }
        elsif ( $TypeList{$_}->{SourceName} eq $TypeList{$_}->{TargetName} ) {
            $KnownLinkTypes{$_} = '=';
        }
        else {
            $KnownLinkTypes{$_} = '>';
        }
    }

    # cache result
    $Self->{KnownLinkTypes} = \%KnownLinkTypes;

    return \%KnownLinkTypes;
}

=item GetKnownObjectTypes()

Gets I<all> known "object" types for the purpose of setting constraints
for link tracing and visualization. 
Currently these are C<Service>, C<ITSMConfigItem> and refinements of it
in the form C<ITSMConfigItem::I<Class>>. 
Object types are returned as the keys of a HASHref for use as lookup table.

    my $HashRef = $TraceObject->GetKnownObjectTypes();

=cut

sub GetKnownObjectTypes {
    my ($Self, %Param) = @_;

    my $GeneralCatalogObject = $Kernel::OM->Get('Kernel::System::GeneralCatalogObject');

    my %KnownObjectTypes = ( 'Service' => 1, 'ITSMConfigItem' => 1, ); # "built-in"

    # add specializations of config items as separate object types
    my $ITSMConfigItemClasses = $GeneralCatalogObject->ItemList(
        Class => 'ITSM::ConfigItem::Class',
        Valid => 1, 
    );

    $KnownObjectTypes{"ITSMConfigItem::$_"} = 1 for values %$ITSMConfigItemClasses;

    return \%KnownObjectTypes;
}
    
=item SetConstraints()

The extent of a trace can be controlled by filtering ITSM object- 
and link-types as well as by setting one of the other constraints
shown below.

For convenience, lists of identifiers can be passed as ARRAY ref 
or as comma-separated string.  Most constraints are not applied 
to an object which initially starts a trace.
   
    my $ItemCount = $TraceObject->SetConstraints(

            # ALL parameters are optional; 
        #  - default for list parameters is "not set" (i.e. not filtered),
        #  - default for boolean/integer parameters is 0

        # Explicit list of link types to include in the trace
        # possible: all known link types (only the "raw" types, not any
        #           direction-dependent labels that they might have).
        LinkTypes  => [ qw( DependsOn RelevantTo AlternativeTo ComposedOf ) ],
        LinkTypes  => 'DependsOn,RelevantTo,AlternativeTo,ComposedOf',

        # Explicit list of object types to include in the trace
        # possible: 'Service', 'ITSMConfigItem', 'ITSMConfigItem::I<Class>'
        ObjectTypes  => [ qw( Service ITSMConfigItem ) ],
        ObjectTypes  => 'Service,ITSMConfigItem',

        # Flag to include objects in the trace that do not have status "valid";
        # this is also respected for objs that start a trace!
        IncludeInvalidObjects  => 0,         # 0 | 1

        # Flag to trace only to/from objects with an incident (non-operational)
        TraceIncident  => 0,             # 0 | 1

        # Number of "hops" to traverse; 0 = unlimited
        MaxTraceDepth => 0,                   # >= 0
    );

If anything goes wrong, the method logs an error and returns C<undef>, 
otherwise it returns 1.

=cut

sub SetConstraints {    
    my ( $Self, %Param ) = @_;

    my $LogObject = $Kernel::OM->Get('Kernel::System::Log');

    # check and collect params that we can process
    if ( $Param{LinkTypes} ) {

        # check for correct data type / split string-list
        $Param{LinkTypes} = $Self->_CheckStringListParam($Param{LinkTypes}, 'LinkTypes');

        return if !defined $Param{LinkTypes};    # error, already logged

        # check requested link types against known ones
        my $KnownLinkTypes = $Self->GetKnownLinkTypes;
        my %LinkTypes;

        for my $RequestedLinkType ( @{$Param{LinkTypes}} ) {
            if ( !exists $KnownLinkTypes->{$RequestedLinkType} ) {

                $LogObject->Log(
                    Priority => 'error',
                    Message  => "Unknown link type '$RequestedLinkType' "
                        . "for parameter 'LinkTypes'! "
                        . "Known types are: \n\t'" 
                        . join("',\n\t'", sort keys %{$KnownLinkTypes})."'",
                );

                return;
            } 

            $LinkTypes{$RequestedLinkType}++;
        } 

        # save HASHref for quick lookup
        $Self->{LinkTypes} = \%LinkTypes;
    }

    if ( $Param{ObjectTypes} ) {
        # check for correct data type / split string-list
        $Param{ObjectTypes} =
            $Self->_CheckStringListParam($Param{ObjectTypes}, 'ObjectTypes');

        return if !defined $Param{ObjectTypes};    # error, already logged

        # check requested object types against known ones
        my $KnownObjectTypes = $Self->GetKnownObjectTypes;
        my %ObjectTypes;

        for my $RequestedObjectType ( @{$Param{ObjectTypes}} ) {
            if ( !exists $KnownObjectTypes->{$RequestedObjectType} ) {
                $LogObject->Log(
                    Priority => 'error',
                    Message  => "Unknown object type '$RequestedObjectType' "
                        . "for parameter 'ObjectTypes'! "
                        . "Known types are: \n\t'" 
                        . join("',\n\t'", sort keys %{$KnownObjectTypes})."'",
                );

                return;
            }

            $ObjectTypes{$RequestedObjectType}++;
        }

        # save HASHref for quick lookup
        $Self->{ObjectTypes} = \%ObjectTypes;
    }

    for my $Key ( qw( IncludeInvalidObjects TraceIncident )) {
        if ( $Param{$Key} && $Param{$Key} !~ m{^[01]$} ) {
            $LogObject->Log(
                Priority => 'error',
                Message  => "'$Key' must be 0 or 1, not '$Param{$Key}'!",
            );

            return;
        }

        $Self->{$Key} = ( ($Param{$Key} || 0) != 0 );
    }

    if ( $Param{MaxTraceDepth} && $Param{MaxTraceDepth} !~ m{^[0-9]+$} ) {
        $LogObject->Log(
            Priority => 'error',
            Message  => "'MaxTraceDepth' must be a non-negative integer, "
                . "not '$Param{MaxTraceDepth}'!",
        );
        return;
    }

    $Self->{MaxTraceDepth} = $Param{MaxTraceDepth} || 0;

    return 1;
}

########################################################################
#
# Private method to ensure that the given parameter is either an ARRAYref
# or a comma/blank-separated list of "identifiers" that is split into an 
# ARRAYref using /\s*[,;]\s*/.
# In case of error, a message is logged and the function returns undef.
#
#     $AryRef = $TraceObject->_CheckStringListParam( $ParamToBeChecked, 
#                                  $ParamName );
#
sub _CheckStringListParam {
    my ($Self, $Param, $Name) = @_;

    my $LogObject = $Kernel::OM->Get('Kernel::System::Log');

    my $Ref = ref $Param;

    # Check for ARRAY ref
    if ( $Ref && $Ref ne 'ARRAY' ) {
        $LogObject->Log(
            Priority => 'error',
            Message  => "Parameter '$Name' must be an ARRAY ref "
                ."or a string-list!",
        );
        return;
    }
    elsif ( !$Ref )  {

        # Check for valid string, split it
        $Param =~ s/^\s+//;    # trim left
        $Param =~ s/\s+$//;    # trim right

        if ( $Param !~ m/^[-_.,;:# a-z0-9]+$/i ) {
            $LogObject->Log(
                Priority => 'error',
                Message  => "Invalid string for parameter '$Name', "
                    ."must be list of identifiers!",
            );
            return;
        }

        $Param = [ split(/\s*[,;]\s*/, $Param) ];
    }

    return $Param;
}

########################################################################
#

=item Trace()

Recursively loads objects and links between them and generates a trace 
which it then renders in one of the supported output formats.

Starting from a single service given by its ID:

    my $Output = $TraceObject->BuildTrace(
        ServiceID    => $ServiceID,        # ID > 0
    ...
    );

Hierarchical nesting of services is expressed by a pseudo-link C<ComposedOf>.

Starting from a single config item given by its ID:

    my $Output = $TraceObject->BuildTrace(
    ConfigItemID    => $ConfigItemID,    # ID > 0
    ...
    );

At least one of the parameters I<ServiceID> or I<ConfigItemID> must be 
specified. An ID of 0 selects all items of this type.

A I<specific> object with which the trace starts is always included,
irrespective of any constraints that otherwise restrict the inclusion
of objects and links into the graph.

The currently supported output formats are: C<text> (plain), C<png> 
(PNG image), C<imgmap> (PNG image plus HTML imagemap), and C<dot> 
(GraphViz "source"). A string of arbitrary output options can also 
be passed to the renderer.

    my $OutputRef = $TraceObject->Trace(
    ...
    );

The output is returned as HASHref, since it can consist of multiple 
parts. For details, see the description of the renderer C<GraphVizRenderer>.

If there is an error, this is logged and the method returns C<''>.

=cut

sub Trace {
    my ( $Self, %Param ) = @_;

    my $LogObject     = $Kernel::OM->Get('Kernel::System::Log');
    my $WrapperObject = $Kernel::OM->Get('Kernel::System::CMDBExplorer::ObjectWrapper');

    # check needed stuff
    if ( ! defined $Param{ServiceID} && ! defined $Param{ConfigItemID} ) {
        $LogObject->Log(
            Priority => 'error',
            Message  => "Need at least one of the parameters 'ServiceID' "
               ."or 'ConfigItemID'!",
        );

        return '';
    }

    $Self->{ServiceID}    = $Param{ServiceID}    if defined $Param{ServiceID};
    $Self->{ConfigItemID} = $Param{ConfigItemID} if defined $Param{ConfigItemID};
    $Self->{DisplayedCIs} = $Param{DisplayedCIs} if defined $Param{DisplayedCIs};

    # Init
    $WrapperObject->Init();    # clear cache
    $Self->GetKnownLinkTypes;    # preload private link type lookup table

    # Start the recursion to build the trace
    $Self->{VisitedObjects} = { };    # loop protection
    my @TraceSteps = ( );    # step includes link + object at "other" end

    for my $Object (
        @{$Self->_ExpandServiceID(    $Self->{ServiceID}    ) },
        @{$Self->_ExpandConfigItemID( $Self->{ConfigItemID} ) },
        @{$Self->_ExpandConfigItemID( $Self->{DisplayedCIs} ) }
    ){

        # Prepare the "root" step of the trace (no link, just Object2)
        my $TraceStep = { 
            Level       => 0,       # recursion level
            LinkType    => '',      # type of link to object
            LinkDiri    => '',      # 'out' | 'in' | '' (=root)
            LinkDirType => '',      # '=' (non-directed)  | '>' | '!' | '+'
            Object1     => undef,   # object at "this" end of link (root step: none)
            Object2     => $Object, # object at "other" end of link
            Position    => 0,       # relative pos. in top/down dep. chain
                                    #   directed outlink: ++, inlink: --
            Visited     => 0,       # flag if Object2 has been visited before
        };

        push @TraceSteps, $TraceStep;

        my $SubTraceSteps = $Self->_FollowLinks( $Object, 1, 0 );
        push @TraceSteps, @{$SubTraceSteps} if @{ $SubTraceSteps || [] };
    }

    # Create output, return it
    my $GraphViz = $Kernel::OM->Get('Kernel::System::CMDBExplorer::GraphVizRenderer');

    $GraphViz->Init(
        Debug        => $Self->{Debug},
        RootCI       => $Self->{ConfigItemID}[0],
        DisplayedCIs => $Self->{DisplayedCIs},
        DisplayedCIs => $Self->{DisplayedCIs},
        Layout       => $Param{Layout} || 'dot',
    );

    return $GraphViz->Render(
        TraceSteps => \@TraceSteps,
    );
}

########################################################################
#
# Private method to recurse into the trace
sub _FollowLinks {
    my ( $Self, $Object, $Level, $Pos ) = @_;

    my $LogObject = $Kernel::OM->Get('Kernel::System::Log');
    
    # Mark object as visited
    $Self->{VisitedObjects}->{$Object}++;
    
    # Get links of current object, follow them
    my @TraceSteps = ( );
    my $IncludeInvalidObjects = $Self->{IncludeInvalidObjects};
    my $LinkList = $Object->GetLinkList;

    LINKEDCLASS:
    for my $LinkedClass ( keys %{ $LinkList || {} } ) {
        if ($LinkedClass eq 'Ticket') {
            1;                # DEBUG: x %{$LinkList->{$LinkedClass}}        
            next LINKEDCLASS; # always skipped
        }

        LINKTYPE:
        for my $LinkType ( keys %{$LinkList->{$LinkedClass} || {} } ) {
            next LINKETYPE if !$Self->_isLinkTypeAllowed($LinkType);    # filtered?

            my $LinkDirType = $Self->{KnownLinkTypes}->{$LinkType};
            my $PosDelta    = ( $LinkDirType eq '=' ) ? 0 : 1;

            # Follow out-links
            # For "top-down flow", we only follow _directed_ outlinks if we are
            # at or "below" the root obj and the link goes "down" or it is
            # a non-directed link
#        if ( $PosDelta && ($Pos>=0) || !$PosDelta ) {
            if ( 1 ) {
                my @TargetIDs = keys %{$LinkList->{$LinkedClass}->{$LinkType}->{Target} || {} };

                TARGETID:
                for my $ID (@TargetIDs) {
                    my $TargetObject = $Object->new(
                        Type => $LinkedClass,
                        ID   => $ID
                    );

                    # Check for dead link
                    if (!$TargetObject) {
                        $LogObject->Log(
                            Priority => 'error',
                            Message  => "Link <<$LinkType>> from "
                                . $Object->ToString()
                                . " to non-existing object with id $ID.",
                        );

                        next TARGETID;
                    } 

                    # Filter by attributes of object
                    next TARGETID if !( $TargetObject->IsValid || $IncludeInvalidObjects );
                    next TARGETID if !$Self->_isObjectTypeAllowed($TargetObject);
                    next TARGETID if !$Self->_isObjectInciStateAllowed($TargetObject);

                    # Link already traversed (in opposite direction)?
                    my $LinkSignature = $Object.$LinkType.$TargetObject;
                    next TARGETID if $Self->{VisitedLinks}->{$LinkSignature};

                    # Prepare trace step for target object
                    my $Visited = $Self->{VisitedObjects}->{$TargetObject};

                    push @TraceSteps, { 
                        Level       => $Level,
                        LinkType    => $LinkType,
                        LinkDir     => 'out',
                        LinkDirType => $LinkDirType,
                        Object1     => $Object,
                        Object2     => $TargetObject,
                        Position    => $Pos,
                        Visited     => $Visited,
                    };

                    # Check for end of recursion
                    next TARGETID if $Visited;        # already seen
                    next TARGETID if $Self->{MaxTraceDepth} && $Level >= $Self->{MaxTraceDepth};

                    # Save link that we just traversed
                    $Self->{VisitedLinks}->{$LinkSignature}++;

                    # Recurse
                    my $SubTraceSteps = $Self->_FollowLinks( 
                        $TargetObject, 
                        $Level+1,
                        $Pos + $PosDelta,    # "down"
                    );

                    push @TraceSteps, @$SubTraceSteps if scalar(@$SubTraceSteps);
                }
            }
                
            # Follow in-links
            # For "top-down flow", we only follow _directed_ inlinks if we are
            # at or "above" (<0) the root obj and the link goes "up" or it is
            # a non-directed link
#        if ( $PosDelta && ( $Pos <= 0 ) || !$PosDelta ) {
            if ( 1 ) {
                my @SourceIDs = keys %{$LinkList->{$LinkedClass}->{$LinkType}->{Source}};

                ID:
                for my $ID (@SourceIDs) {
                    my $SourceObject = $Object->new(
                        Type => $LinkedClass,
                        ID => $ID
                    );

                    if (!$SourceObject) {
                        $LogObject->Log(
                            Priority => 'error',
                            Message  => "Link <<$LinkType>> to "
                                . $Object->ToString()
                                . " from non-existing object with id $ID.",
                        );

                        next ID;        # skip
                    }

                    # Filter by attributes of object
                    next ID if !( $SourceObject->IsValid || $IncludeInvalidObjects );
                    next ID if !$Self->_isObjectTypeAllowed($SourceObject);
                    next ID if !$Self->_isObjectInciStateAllowed($SourceObject);

                    # Link already traversed (in opposite direction)?
                    my $LinkSignature = $SourceObject.$LinkType.$Object;
                    next ID if $Self->{VisitedLinks}->{$LinkSignature};

                    # Prepare trace step for source object
                    my $Visited = exists $Self->{VisitedObjects}->{$SourceObject};
                    push @TraceSteps, { 
                        Level       => $Level,
                        LinkType    => $LinkType,
                        LinkDir     => 'in',
                        LinkDirType => $LinkDirType,
                        Object1     => $Object,
                        Object2     => $SourceObject,
                        Position    => $Pos,
                        Visited     => $Visited,
                    };

                    # Save link that we just traversed
                    $Self->{VisitedLinks}->{$LinkSignature}++;

                    # Recurse if
                    #   - no loop
                    #   - no trace depth limit or within this limit
                    #   - not top-down only or this was a directed link
                    if (
                        ! $Visited &&
                        ( ! $Self->{MaxTraceDepth} ||
                            $Self->{MaxTraceDepth} && $Level < $Self->{MaxTraceDepth}
                        )
                    ) {
                        my $SubTraceSteps = $Self->_FollowLinks( 
                            $SourceObject, 
                            $Level+1,
                            $Pos - $PosDelta,    # "up"
                        );

                        push @TraceSteps, @$SubTraceSteps if scalar(@$SubTraceSteps);
                    }
                }
            }
        }
    }

    return \@TraceSteps;
} 

########################################################################
#
# Private method to render a trace as plaintext
sub _RenderAsText {
    my ($Self, $TraceSteps) = @_;
    my @Text;
    for my $Step ( @$TraceSteps ) {

        my $Link = '';
        if ( $Step->{LinkType} ) {
            if ($Step->{LinkDirType} eq '=') {
                $Link = '<-->';        # non-directed
            }
            else {
                $Link = $Step->{LinkDir} eq 'in' ? '<-' : '->';
            }

            $Link .= "  <<" . $Step->{LinkType} . ">>  ";
        }

        my $ClusterInfo = '';
        if (exists $Self->{Object2Scope}->{$Step->{Object2}}) {
            my $Scope =  $Self->{Object2Scope}->{$Step->{Object2}};
            $ClusterInfo = ' [' . $Scope->ToString . ']';
        }

        my $Text = ("  " x $Step->{Level})    # indent
             . $Link            # link to/from Object2
             . $Step->{Object2}->ToString
             . $ClusterInfo
             . ($Step->{Visited} ? ' (already visited)' : '');

        push @Text, $Text;
    }

    return { text => join( "\n", @Text, '') };
}

########################################################################
#
# Private method to load initial service object(s), returns ARRAY ref.
# No filtering except that a "wildcard" (ID=0) respects the setting
# of 'IncludeInvalidObjects'.
sub _ExpandServiceID {
    my ($Self, $ServiceID) = @_;

    return [] unless $ServiceID;

    my $WrapperObject = $Kernel::OM->Get('Kernel::System::CMDBExplorer::ObjectWrapper');
    my $ServiceObject = $Kernel::OM->Get('Kernel::System::Service');

    my @Objects;
    if ($ServiceID > 0) {
        # Single service, try to load it

        my $Object = $WrapperObject->GetObject(
            Type => 'Service', 
            ID   => $Self->{ServiceID}
        );

        push @Objects, $Object if $Object;
    }
    else { 

        # All known services
        my %ServiceList = $ServiceObject->ServiceList(
            Valid  => ! $Self->{IncludeInvalidObjects},
            UserID => 1,
        );

        for my $ID ( sort keys %ServiceList ) {
            my $Object = $WrapperObject->GetObject(
                Type => 'Service', 
                ID   => $ID,
            );

            push @Objects, $Object if $Object;
        } 

        # Sort services by "rank" (top-level services first)
        # This gives _much_ more useful text output
        @Objects = sort { $a->GetRank <=> $b->GetRank } @Objects;
    }

    return \@Objects;
}

########################################################################
#
# Private method to load initial config item(s); returns ARRAY ref.
# Explicitly given CIs (by ID) are not filtered, otherwise filtering
# respects IncludeInvalidObjects and ObjectTypes.
sub _ExpandConfigItemID {
    my ($Self, $ConfigItemID) = @_;

    return [] unless @{ $ConfigItemID }[0] ne '';

    my @Objects = ( );

    my $IncludeInvalidObjects = $Self->{IncludeInvalidObjects};

    my $WrapperObject    = $Kernel::OM->Get('Kernel::System::CMDBExplorer::ObjectWrapper');
    my $ConfigItemObject = $Kernel::OM->Get('Kernel::System::ITSMConfigItem');

    CI:
    for my $CI ( @{ $ConfigItemID } ) {
        if ($CI != 0) {

            # Single config item, try to load it

            my $Object = $WrapperObject->GetObject(
                Type => 'ITSMConfigItem', 
                ID   => $CI,
            );

            next CI if !$Object;
            next CI if !( $Object->IsValid || $IncludeInvalidObjects );
            next CI if !$Self->_isObjectTypeAllowed($Object);

            push (@Objects, $Object) if $Object;
        }
        else { 

            # All config items; load & filter
            my $ConfigItemList        = $ConfigItemObject->ConfigItemSearch();
            my $IncludeInvalidObjects = $Self->{IncludeInvalidObjects};

            for my $ID ( @{$ConfigItemList} ) {
                my $Object = $WrapperObject->GetObject(
                    Type => 'ITSMConfigItem', 
                    ID   => $ID,
                );

                next CI if !$Object;
                next CI if !( $Object->IsValid || $IncludeInvalidObjects );
                next CI if !$Self->_isObjectTypeAllowed($Object);

                push (@Objects, $Object);
            } 
        } 
    }

    return \@Objects;
} 

# Private method to check if a given link type meets the current constraints
sub _isLinkTypeAllowed {
    my ($Self, $LinkType) = @_;

    return 1 unless exists $Self->{LinkTypes};        # not filtered
    return 1 if $Self->{LinkTypes}->{$LinkType};    

    my $LogObject = $Kernel::OM->Get('Kernel::System::Log');

    if ($Self->{Debug}) {
        $LogObject->Log(
            Priority => 'debug',
            Message  => "Skipping filtered link type '$LinkType'.",
        );
    }

    return 0;
}

# Private method to check if a given object meets the current
# type constraints
sub _isObjectTypeAllowed {
    my ($Self, $Object) = @_;

    return 1 unless exists $Self->{ObjectTypes};        # not filtered
    return 1 if $Self->{ObjectTypes}->{$Object->GetType};    # CI w/o class
    return 1 if $Self->{ObjectTypes}->{$Object->GetFullType};

    my $LogObject = $Kernel::OM->Get('Kernel::System::Log');

    if ($Self->{Debug}) {
        my $T = $Object->GetFullType;
        $LogObject->Log(
            Priority => 'debug',
            Message  => "Skipping filtered object type '$T'.",
        );
    }

    return 0;
}

# Private method to check if a given object meets the current 
# incident state constraints (when following only "hot" links)
sub _isObjectInciStateAllowed {
    my ($Self, $Object) = @_;

    return 1 unless $Self->{TraceIncident};
    return 1 if $Object->GetCurInciState ne 'operational';
    return 0;
}

1;

=back

=head1 TERMS AND CONDITIONS

Copyright (C) 2018- Perl-Services.de, http://perl-services.de
Copyright (C) 2014-2018 Belnet, http://www.belnet.be/
Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/

This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 

For license information, see the enclosed file COPYING-CMDBExplorer
(GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
If you did not receive this file, see 
http://www.gnu.org/licenses/agpl-3.0.html.


=head1 AUTHOR

info@perl-services.de
cyrille.bollu@belnet.be
dietmar.berg@thalesgroup.com

=cut
