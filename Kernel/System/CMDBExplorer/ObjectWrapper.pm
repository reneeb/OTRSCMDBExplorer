# --
# Copyright (C) 2018- Perl-Services.de, http://perl-services.de
# Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/
# --
# This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 
# For license information, see the enclosed file COPYING-CMDBExplorer
# (GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
# If you did not receive this file, see 
# http://www.gnu.org/licenses/agpl-3.0.html.
# --

package Kernel::System::CMDBExplorer::ObjectWrapper;

=head1 NAME

Kernel::System::CMDBExplorer::ObjectWrapper - render trace through graphviz library

=head1 SYNOPSIS

Wrapper class for 'Service' and 'ITSMConfigItem' to expose
both of them with a common interface for tracing links.
Each unique CI or service is instantiated just once; 
subsequent constructor calls return a reference to the same object.

=cut

use strict;
use warnings;

our @ObjectDependencies = qw(
    Kernel::Config
    Kernel::System::Log
    Kernel::System::Main
    Kernel::System::ITSMConfigItem
    Kernel::System::GeneralCatalog
    Kernel::System::LinkObject
);

# Cache for instances of this class
our %ObjectInstances;

# HACK: Static values to define "validity" of services
my $SERVICE_VALID_ID = 1;    # ID for "valid" 

# Categorization of ITSM::ConfigItem::DeploymentState
# (tables general_catalog_class + general_catalog_preferences).
# Loading deferred until we have access to a GeneralCatalog object.
my $CI_VALID_DEPLOYMENT_STATES;

=head1 PUBLIC INTERFACE

=over 4

=cut

=item Kernel::System::CMDBExplorer::ObjectWrapper::Init()

Static method to initialize the cache built into the class.

=cut

sub Init { 
    delete $ObjectInstances{$_} for keys %ObjectInstances;
    undef %ObjectInstances;     # TODO -- HACK!
}

=item Kernel::System::CMDBExplorer::ObjectWrapper::GetAllInstances()

Static method that returns all instances as HASHref, with keys
I<ObjectType>#I<ID>.

=cut

sub GetAllInstances {
    my $ClassOrSelf = shift;
    return \%ObjectInstances;
} # GetAllInstances()

=item new()

Returns an instance of an ObjectWrapper for an ITSM object.
Instances are cached within this class; a call to the constructor
is guaranteed to always return the same instance for a given
type/ID combination.

    my $WrapperObject = Kernel::System::CMDBExplorer::ObjectWrapper->new(
    %CommonObjects,
     Debug => 0,        # optional, { 0 | 1 }
    Type => $String        # { Service | ITSMConfigItem | FAQ }
    ID   => $ID        # integer
    );

As a shortcut, objects can be created off an existing one with the 
common objects and debug settings taken from there:

    my $ObjectWrapper = $AnotherObjectWrapper->new(
    Type => $String        # { Service | ITSMConfigItem | FAQ }
    ID   => $ID        # integer
    );

=cut

sub new {
    my ($ClassOrType, %Param) = @_;

    # Construct new object in any case
    my $Self = { };
    my $Class = ref $ClassOrType || $ClassOrType;
    bless ($Self, $Class);

    return $Self;
}

sub GetObject {
    my ($Self, %Param) = @_;

    my $LogObject            = $Kernel::OM->Get('Kernel::System::Log');
    my $GeneralCatalogObject = $Kernel::OM->Get('Kernel::System::GeneralCatalog');

    # Check required Type/ID
    my $Type = $Param{Type} || '';
    my $ID   = $Param{ID}   || '';
    my $Key  = "$Type#$ID";

    if ($Type =~ /^([^#]+)#(\d+)$/) {
        $Key  = $Type;
        $Type = $1;
        $ID   = $2;
    }

    if ($Type ne 'Service' && $Type ne 'ITSMConfigItem') {
        $LogObject->Log( 
            Priority => 'error',
            Message  => "Unknown type '$Type'",
        ) if $Type eq 'FAQ'; # known, but not [yet?] supported

        return;
    }

    # Try to reuse existing instance from cache
    my $Obj = $ObjectInstances{$Key};

    return $Obj if $Obj;

    # Perform deferred loading
    if ( !defined $CI_VALID_DEPLOYMENT_STATES ) {
        $CI_VALID_DEPLOYMENT_STATES = $GeneralCatalogObject->ItemList(
            Class         => 'ITSM::ConfigItem::DeploymentState',
            Preferences   => { Functionality => [ 'preproductive', 'productive' ], },
        );
    }

    # Load object from DB, through internal cache.
    return if !(
        ($Type eq 'Service'        && $Self->_LoadService($ID)) ||
        ($Type eq 'ITSMConfigItem' && $Self->_LoadITSMConfigItem($ID))
    );

    # Save to cache
    $Self->{Key}  = $Key;
    $Self->{Type} = $Type;
    $Self->{ID}   = $ID;

    $ObjectInstances{$Key} = $Self;

    return $Self;
} 

# Private method to load an ITSM Service into a 
# Kernel::System::CMDBExplorer::ObjectWrapper
#     $ObjectWrapper->_loadService( $ID );
sub _LoadService {
    my ($Self, $ID) = @_;

    my $LogObject     = $Kernel::OM->Get('Kernel::System::Log');
    my $ServiceObject = $Kernel::OM->Get('Kernel::System::Service');

    my $ServiceName = $ServiceObject->ServiceLookup(
        ServiceID => $ID,
    );

    if (!$ServiceName) {
        $LogObject->Log( 
            Priority => 'error',
            Message  => "Cannot load Service #$ID",
        );

        return;
    }

    my %Service = $ServiceObject->ServiceGet(
        ServiceID => $ID,
        UserID    => 1,
    );

    # Save underlying object
    $Self->{Service} = \%Service;

    # Copy "interesting" values
    $Self->{Valid}           = ($Service{ValidID} == $SERVICE_VALID_ID);
    $Self->{Name}            = $Service{Name};
    $Self->{ShortName}       = $Service{NameShort} || $Service{Name};
    $Self->{FullType}        = 'Service';
    $Self->{ServiceParentID} = $Service{ParentID};
    $Self->{CurInciState}    = $Service{CurInciState};

    return $Self;
}

# Load an ITSMConfigItem into a Kernel::System::CMDBExplorer::ObjectWrapper
sub _LoadITSMConfigItem {
    my ($Self, $ID) = @_;

    my $LogObject        = $Kernel::OM->Get('Kernel::System::Log');
    my $ConfigItemObject = $Kernel::OM->Get('Kernel::System::ITSMConfigItem');

    my $CI = $ConfigItemObject->ConfigItemGet(
        ConfigItemID => $ID,
        Cache        => 1,
    );

    if (!$CI) {
        $LogObject->Log( 
            Priority => 'error',
            Message  => "Cannot load ITSMConfigItem #$ID",
        );

        return;
    }

    # Get the "current version" of the CI
    my $Vers   = $CI->{LastVersionID};
    my $CIVers = $ConfigItemObject->VersionGet(
        VersionID  => $Vers,
        XMLDataGet => 0,    # (optional) default 1 (0|1)
    );

    # Dont' process CI with invalided class
    $CIVers->{Class} || return;

    # Save underlying object
    $Self->{ITSMConfigItem}->{LastVersion} = $CIVers;

    # Copy "interesting" values
    $Self->{Name}      = $CIVers->{Name};
    $Self->{ShortName} = $CIVers->{Name};
    $Self->{FullType}  = 'ITSMConfigItem::'.($CIVers->{Class}||'');

    # Define validity through deployment state
    $Self->{Valid} = defined $CI_VALID_DEPLOYMENT_STATES->{$CIVers->{CurDeplStateID}};

    # Get incident state
    $Self->{CurInciState} = $CIVers->{CurInciState};

    return $Self;
} 

###  M e t h o d s  ####################################################
#
 
=item GetID()

returns ID of wrapped ITSM object


=item GetKey()

returns I<Type>#I<ID> of the wrapped ITSM object


=item GetType()

returns "basic" type of the wrapped ITSM object


=item GetFullType()

for a CI only, returns C<ITSMConfigItem::> + class of CI


=item GetName(), GetShortName()

returns full name or just last segment (services only)


=item GetCurInciState()

returns incident state of service or CI


=item GetServiceParentID()

for services only, returns ID of parent (null if top-level service)


=item IsValid()

returns true if this object is "valid" in the sense of OTRS

=cut


# Simple accessors
sub GetID              { return $_[0]->{ID};                    }
sub GetKey             { return $_[0]->{Key};                   }
sub GetType            { return $_[0]->{Type}            || ''; }
sub GetFullType        { return $_[0]->{FullType}        || ''; }
sub GetName            { return $_[0]->{Name}            || ''; }
sub GetShortName       { return $_[0]->{ShortName}       || ''; }
sub GetCurInciState    { return $_[0]->{CurInciState}    || ''; }
sub GetServiceParentID { return $_[0]->{ServiceParentID} || ''; }
sub IsValid            { return $_[0]->{Valid};                 }


=item ToString()

returns string representation of this object

=cut

sub ToString {
    my $Self = shift;
    return  sprintf '%s [%s#%s]',
        $Self->GetShortName,
        $Self->GetFullType,
        $Self->GetID;
}

########################################################################
#

=item GetLinks()

returns all links of this object as retrieved through a C<LinkObject>.
For hierarchically nested services, these links are amended by pseudo
links C<ComposedOf>.

=cut

sub GetLinkList {
    my $Self = shift;

    return {} unless $Self->IsValid;

    # Already loaded?
    my $LinkList = $Self->{LinkList};
    return $LinkList if $LinkList;

    my $LinkObject = $Kernel::OM->Get('Kernel::System::LinkObject');

    # Load from DB, store in object
    $LinkList = $LinkObject->LinkList(
        Object => $Self->{Type},    # table 'link_object'
        Key    => $Self->{ID},
        State  => 'Valid',
        UserID => 1,
    );

    # Special hack for 'Service': Expose decomposition as links
    if ($Self->{Type} eq 'Service') {
        # Are we child of another service?
        if ($Self->{Name} ne $Self->{ShortName}) {
            my $ParentName = substr( $Self->{Name}, 0, -(2+length ($Self->{ShortName})) );
            my $ParentID   = $Self->{ServiceObject}->ServiceLookup(Name => $ParentName);

            $LinkList->{Service}->{ComposedOf}->{Source}->{$ParentID}++;
        }

        # Do we ourselves have children?
        my @ServiceIDs = $Self->{ServiceObject}->ServiceSearch(
            Name   => $Self->{Name}."::%",
            Limit  => 100,
            UserID => 1,
        );

        CHILDID:
        for my $ChildID (@ServiceIDs) {
            my $Object = $Self->new(
                Type => 'Service',
                ID => $ChildID
            );

            # Only consider immediate children
            next CHILDID if $Object->GetServiceParentID != $Self->{ID};
            $LinkList->{Service}->{ComposedOf}->{Target}->{$ChildID}++;
        }
    }

    $Self->{LinkList} = $LinkList;

    return $LinkList;
}

#
########################################################################
########################################################################
#

=item GetRank()

returns "rank" of object in the hierarchy.
Only meaningful for services, with the top-level being at rank 1.
For all other objects, the method returns 0.

=cut 

sub GetRank {
    my $Self = shift;

    return 0 if !$Self->IsValid;
    return $Self->{Rank} if exists $Self->{Rank};

    my $Rank = 0;    # anything other than a service has rank 0

    if ($Self->{Type} eq 'Service') {

        # For services, we just need to count the number of "::" 
        ### Bug spotted by Andrey Sidorenko <andrey@sidorenko.spb.ru> 
        my @Svcs = split( /::/, $Self->{Name} );
        $Rank = scalar @Svcs;
    }

    $Self->{Rank} = $Rank;    # save it for reuse

    return $Rank;
}

#
########################################################################
########################################################################
#
# Get total # of outlinks from this item (recursively).
# Items that are visited multiple times are counted fully 
# for the first visit, 1 for each additional one.
sub GetOutlinkCountTotal {
    my $Self = shift;
    # Already calculated?
    my $Count = $Self->{OutlinkCountTotal};
    return $Count if defined $Count;

    # Calculate, store in object
    $Count = 0;
    my $LinkList = $Self->GetLinkList();

    if ($LinkList) {
        $Count = $Self->_CountOutlinksRecursive({});
    }

    $Self->{OutlinkCountTotal} = $Count;

    return $Count;
} # GetOutlinkCountTotal()


# Recursively count out-links from current item to CIs, Services. 
# Useful for presentation of objects by relative "importance".
sub _CountOutlinksRecursive {
    my ($Object, $Visited) = @_;

    # Loop protection
    return 1 if $Visited->{$Object};

    $Visited->{$Object}++;

    my $LinkList = $Object->GetLinkList();
    return 0 if !$LinkList;

    my $Count = 0;
    for my $LinkedClass (keys %$LinkList) {
        for my $LinkType (keys %{$LinkList->{$LinkedClass}}) {
            my @TargetIDs = keys %{$LinkList->{$LinkedClass}->{$LinkType}->{Target}};

            ID:
            for my $ID (@TargetIDs) {
                my $TargetObject = $Object->new(
                    Type => $LinkedClass,
                    ID   => $ID,
                );

                next ID if !$TargetObject;

                if ( $TargetObject->IsValid ) {
                    $Count += 1 + $TargetObject->_CountOutlinksRecursive($Visited)
                }
            }
        }
    }

    return $Count;
}

#
#########################################################################
########################################################################
#
# Get total # of inlinks to this item (recursively).
# Items that are visited multiple times are counted fully 
# for the first visit, 1 for each additional one.
sub GetInlinkCountTotal {
    my $Self = shift;

    # Already calculated?
    my $Count = $Self->{InlinkCountTotal};
    return $Count if defined $Count;

    # Calculate, store in object
    $Count = 0;

    my $LinkList = $Self->GetLinkList();
    if ($LinkList) {
        $Count = $Self->_CountInlinksRecursive({});
    }

    $Self->{InlinkCountTotal} = $Count;

    return $Count;
}


# Recursively count in-links to current item from CIs, Services. 
# Useful for presentation of objects by relative "importance".
sub _CountInlinksRecursive {
    my ($Object, $Visited) = @_;

    # Loop protection
    return 1 if $Visited->{$Object};

    $Visited->{$Object}++;
    my $LinkList = $Object->GetLinkList();

    return 0 unless $LinkList;

    my $Count = 0;
    for my $LinkedClass (keys %$LinkList) {
        for my $LinkType (keys %{$LinkList->{$LinkedClass}}) {
            my @SourceIDs = keys %{$LinkList->{$LinkedClass}->{$LinkType}->{Source}};

            ID:
            for my $ID (@SourceIDs) {
                my $SourceObject = $Object->new(
                    Type => $LinkedClass,
                    ID   => $ID,
                );

                next ID if !$SourceObject;

                if ( $SourceObject->IsValid ) {
                    $Count += 1 + $SourceObject->_CountInlinksRecursive($Visited);
                }
            }
        }
    }

    return $Count;
}

#
#########################################################################

1;

=back

=head1 TERMS AND CONDITIONS

Copyright (C) 2018- Perl-Services.de, http://perl-services.de
Copyright (C) 2011-2014 Thales Austria GmbH, http://www.thalesgroup.com/

This software comes with ABSOLUTELY NO WARRANTY and WITHOUT ANY SUPPORT. 

For license information, see the enclosed file COPYING-CMDBExplorer
(GNU AFFERO GENERAL PUBLIC LICENSE, version 3). 
If you did not receive this file, see 
http://www.gnu.org/licenses/agpl-3.0.html.


=head1 AUTHOR

info@perl-services.de
dietmar.berg@thalesgroup.com

=cut
