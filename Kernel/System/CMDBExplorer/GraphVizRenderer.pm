# --
# Copyright (C) 2018- Perl-Services.de, http://perl-services.de
# Copyright (C) 2014-2018 Belnet, http://www.belnet.be
# Copyright (C) 2011 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-CMDBExplorer
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.
# --

package Kernel::System::CMDBExplorer::GraphVizRenderer;

=head1 NAME

Kernel::System::CMDBExplorer::GraphVizRenderer - render trace through graphviz library

=head1 SYNOPSIS

This class encapsulates the graphical rendering of a trace (represented
by an array of trace steps) through the graphivz library from 
http://www.graphviz.org.

=cut

use strict;
use warnings;

use GraphViz;

our @ObjectDependencies = qw(
    Kernel::Config
    Kernel::System::Service
    Kernel::System::ITSMConfigItem
    Kernel::System::LinkObject
    Kernel::System::GeneralCatalog
    Kernel::System::CMDBExplorer::ObjectWrapper
);

########################################################################
# 
# Mapping of object/link types to visual representation
#
our %InciStateColors = (
    Warning    => '#FFDD50', # yellow
    Incident    => '#FF505E', # red
);

our %ClusterAttrs = (
    color => '#444444',
    penwidth => 3,
);

=head1 PUBLIC INTERFACE

=over 4

=cut

###  C o n s t r u c t o r  ############################################
#

=item new()

Creates an object

    my $GraphVizRenderer = Kernel::System::CMDBExplorer::GraphVizRenderer->new();

=cut

sub new {
    my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {};
    bless( $Self, $Type );

    # Get config
    my $ConfigObject = $Kernel::OM->Get('Kernel::Config');

    $Self->{GraphOptions}  = $ConfigObject->Get("CMDBExplorer::GraphOptions");
    $Self->{LayoutOptions} = $ConfigObject->Get("CMDBExplorer::GraphOptions::LayoutOptions");

    return $Self;
}

sub Init {
    my ($Self, %Param) = @_;

    $Self->{Debug}        = $Param{Debug} || 0;
    $Self->{RootCI}       = $Param{RootCI} || 0;
    $Self->{DisplayedCIs} = $Param{DisplayedCIs};
    $Self->{Layout}       = $Param{Layout} || 'dot';

    return 1;
}

###  M e t h o d s  ####################################################
#

=item Render()

Renders the graph given by C<$TraceSteps> through graphviz with
the built-in mapping from objects & links to graphical elements. 

    $TraceSteps = [ 
    { 
        Level       => $Level,          # recursion level
        LinkType    => $LinkType,       # type of link to object
        LinkDir     => 'in',            # 'out' | 'in' | '' (=root)
        LinkDirType => $LinkDirType,    # '>' | '='
        Object1     => $Object,         # object at "this" end of link
        Object2     => $TargetObject,   # object at "other" end of link
        Position    => $RelPosition,    # relative pos. in dep. chain
        Visited     => $Visited,        # flag if ob has been visited before
    },
    {
        ...
    }, 
    ...
    ];

    # Object-to-scope mapping (see Kernel::System::CMDBExplorer::Scope)
    $Object2Scope = { $Object => $Scope, ... }

    my $Output = $GraphVizRenderer->Render(
        TraceSteps    => $TraceSteps,
    );

    # $Output->{png} now contains the PNG image of the graph, 
    # $Output->{map} contains the client-side image map for it.

=cut

sub Render {
    my ($Self, %Param) = @_;

    my $TraceSteps   = $Param{TraceSteps} || [ ];

    # Turn trace steps into (unique) nodes and edges
    my %Nodes;
    my %Edges;

    for my $TraceStep ( @$TraceSteps ) {

        # Record all objects as nodes
        my $Object1 = $TraceStep->{Object1};
        my $Object2 = $TraceStep->{Object2};
        my $LinkDir = $TraceStep->{LinkDir};

        $Nodes{$Object2} = $Object2;    # get unique objs (obj2 is enough)

        # Record all links as edges.
        # OTRS always has 2 entries for each link (from either side), 
        # we need to make sure that each link is drawn just once.
        my $LinkType = $TraceStep->{LinkType};
        if ($LinkType) {    # "root" steps don't have a link!
            my $Source = $LinkDir eq 'out' ? $Object1 : $Object2;
            my $Target = $LinkDir eq 'out' ? $Object2 : $Object1;
            $Edges{$Source.$LinkType.$Target} = { 
                Source      => $Source, 
                LinkType    => $LinkType, 
                LinkDirType => $TraceStep->{LinkDirType}, 
                Target      => $Target,
            } if !exists $Edges{$Target.$LinkType.$Source};    
        }
    }

    # Count hierarchy levels of services for later layout control
    my $MaxSvcLevel = 0;

    OBJECT:
    for my $Object (values %Nodes) {
        next OBJECT if !$Object->GetType eq 'Service';

        my $Name     = $Object->GetName;
        my $Level    = scalar (my @x = split /::/, $Name);
        $MaxSvcLevel = $Level if $Level > $MaxSvcLevel;
    }

    $Self->{MaxSvcLevel} = $MaxSvcLevel;

    # Set selected layout's options from SysConfig
    my $LayoutOptions = $Self->{LayoutOptions}->{$Self->{Layout}} || {};

    # Prepare graph
    my $GraphViz = GraphViz->new(
        name    => 'trace',
        layout  => $Self->{Layout},
        %{ $LayoutOptions || {} },
    );

    $Self->{GraphVizObject} = $GraphViz;

    # Render nodes
    for my $Object (values %Nodes) {
        $Self->_RenderObject($Object);
    } 

    # Add edges
    for my $Link (values %Edges) {
        $Self->_RenderLink($Link);
    } 

    # Produce output
    my $Output = { };
    $Output->{map} = $GraphViz->as_cmapx;
    $Output->{png} = $GraphViz->as_png;

    return $Output;
} 

########################################################################
# Private method that controls the rendering of a single object 
# as a GraphViz node.
sub _RenderObject {
    my ($Self, $Object) = @_;

    my %Attrs;

    my %Opts = %{ $Self->{GraphOptions} || {} };

    # Set default node attributes
    $Attrs{shape}     = $Opts{NodeShapes}->{$Object->GetFullType};
    $Attrs{fontsize}  = $Opts{NodeFontSize}         || 8;
    $Attrs{color}     = $Opts{NodeDefaultColor}     || 'LightSteelBlue4';
    $Attrs{fillcolor} = $Opts{NodeDefaultFillColor} || 'white';
    $Attrs{height}    = 0.1;
    $Attrs{width}     = 0.2;
    $Attrs{margin}    = "0.03,0.03";
    $Attrs{style}     = 'filled';

    # Type-specific refinement
    my $Type = $Object->GetFullType;
    if ( $Type eq 'Service' ) {
        my $Name = $Object->GetName;
        my $Level = scalar (my @x = split /::/, $Name);

        $Attrs{URL}     = 'index.pl?Action=AgentITSMServiceZoom;ServiceID='.$Object->GetID;
        $Attrs{tooltip} = "Service: " . __Escape($Object->GetName);
    }
    elsif ( $Type =~ m/^ITSMConfigItem/ ) {
        my $ID = $Object->GetID;

        # URL init
        my $URL ='index.pl?Action=AgentITSMConfigItemZoom;ConfigItemID=';
        my $UrlDisplayedCIs=';DisplayedCIs=';

        if ( $Self->{DisplayedCIs} ) {
            $UrlDisplayedCIs .= join(',' , @{ $Self->{DisplayedCIs} });
        }

        # Tooltip init
        my $Tooltip = $Type;
        $Tooltip =~ s/^.*:://; 
        $Tooltip .= ': ';
        $Tooltip .= __Escape($Object->GetName);

        # Change attributes depending on CI
        if ( $ID eq $Self->{RootCI} ) {
            $Attrs{fillcolor} = $Self->{GraphOptions}->{RootNodeColor} || 'LightSteelBlue2';
            $URL .= $ID;
            $Tooltip .= " (Root CI)";
        }
        elsif ( $ID ~~  @{ $Self->{DisplayedCIs} } ) {
            # Clicking on the node will remove it from the graph
            $Attrs{fillcolor} = $Self->{GraphOptions}->{DisplayedNodeColor} || 'LightSteelBlue1';
            $URL .= $Self->{RootCI};
            $UrlDisplayedCIs =~ s/,$ID//;
            $UrlDisplayedCIs =~ s/$ID,//;
            $Tooltip .= " (Click to remove from graph)";
        }
        else {
            # Clicking on the node will add it from the graph
            $URL             .= $ID;
            $UrlDisplayedCIs .= ',' . $ID;
            $Tooltip         .= " (Click to add to graph)";
        }

        $URL .= $UrlDisplayedCIs;
        $URL .= ';Layout=' . $Self->{Layout};

        # Add URL attribute
        $Attrs{URL} = $URL;

        # Add tooltip attribute
        $Attrs{tooltip} = $Tooltip;
    }

    # Visually mark invalid items
    $Attrs{style} = 'diagonals' unless $Object->IsValid;

    # Visually mark non-operational state
    my $InciStateColor = $InciStateColors{$Object->GetCurInciState};
    $Attrs{color}      = $InciStateColor if $InciStateColor;

    # Add node to graph
    $Self->{GraphVizObject}->add_node(
        $Object->GetKey, 
        label => __Escape($Object->GetShortName), 
        %Attrs
    );
}

########################################################################
#
# Private method that controls the rendering of a single link as an edge
sub _RenderLink {
    my ($Self, $Link) = @_;

    my %Attrs;

    my %Opts = @{ $Self->{GraphOptions} || {} };

    # Set default link attributes
    $Attrs{style}     = $Opts{LinkStyles}->{$Link->{LinkType}} || 'filled';
    $Attrs{dir}       = $Opts{LinkArrows}->{$Link->{LinkType}} || 'none';
    $Attrs{fontsize}  = $Opts{LinkFontSize}                    || 6;
    $Attrs{color}     = $Opts{LinkDefaultColor}                || 'LightSteelBlue4';
    $Attrs{fontcolor} = $Opts{LinkDefaultFontColor}            || 'LightSteelBlue4';
    $Attrs{label}     = $Link->{LinkType} if $Opts{DisplayLinksName};

    # Mark links between CI in non-operational state
    if ( $Link->{LinkDirType} eq '!' ) {
        for my $Node ( 'Source', 'Target' ) {
            if ( $Link->{$Node}->GetCurInciState eq 'Incident' ) {
                $Attrs{color} = $InciStateColors{Incident};
                last;
            } elsif ( $Link->{$Node}->GetCurInciState eq 'Warning' ) {
                $Attrs{color} = $InciStateColors{Warning};
            }
        }
    }

    # Add graphviz edge
    $Self->{GraphVizObject}->add_edge(
        $Link->{Source}->GetKey => $Link->{Target}->GetKey,
        tooltip => $Link->{LinkType},
        %Attrs,
    );
}

########################################################################
#
# Function to protect quotes (i.e. convert " to ')
sub __Escape($) { 
    my $S = shift;
    $S =~ s/"/'/g;
    return $S;
}

1;

=back

=head1 TERMS AND CONDITIONS

Copyright (C) 2018- Perl-Services.de, http://perl-services.de
Copyright (C) 2014-2018 Belnet, http://www.belnet.be
Copyright (C) 2011 Thales Austria GmbH, http://www.thalesgroup.com/

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
