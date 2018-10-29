# --
# Copyright (C) 2001-2017 OTRS AG, http://otrs.com/
# --
# This software comes with ABSOLUTELY NO WARRANTY. For details, see
# the enclosed file COPYING for license information (AGPL). If you
# did not receive this file, see http://www.gnu.org/licenses/agpl.txt.
# --

package Kernel::Modules::AgentITSMServiceZoom;

use strict;
use warnings;

use Kernel::Language qw(Translatable);

our $ObjectManagerDisabled = 1;

###### OTRSCMDBExplorer ######
use MIME::Base64;
###### OTRSCMDBExplorer ######

sub new {
    my ( $Type, %Param ) = @_;

    # allocate new hash for object
    my $Self = {%Param};
    bless( $Self, $Type );

    return $Self;
}

sub Run {
    my ( $Self, %Param ) = @_;

    # get params
    my $ServiceID = $Kernel::OM->Get('Kernel::System::Web::Request')->GetParam( Param => 'ServiceID' );

    # get layout object
    my $LayoutObject = $Kernel::OM->Get('Kernel::Output::HTML::Layout');

    # check needed stuff
    if ( !$ServiceID ) {
        return $LayoutObject->ErrorScreen(
            Message => Translatable('No ServiceID is given!'),
            Comment => Translatable('Please contact the administrator.'),
        );
    }

    # get service
    my %Service = $Kernel::OM->Get('Kernel::System::Service')->ServiceGet(
        ServiceID     => $ServiceID,
        IncidentState => 1,
        UserID        => $Self->{UserID},
    );
    if ( !$Service{ServiceID} ) {
        return $LayoutObject->ErrorScreen(
            Message => $LayoutObject->{LanguageObject}->Translate( 'ServiceID %s not found in database!', $ServiceID ),
            Comment => Translatable('Please contact the administrator.'),
        );
    }

    # get config object
    my $ConfigObject = $Kernel::OM->Get('Kernel::Config');

    # run config item menu modules
    if ( ref $ConfigObject->Get('ITSMService::Frontend::MenuModule') eq 'HASH' ) {
        my %Menus   = %{ $ConfigObject->Get('ITSMService::Frontend::MenuModule') };
        my $Counter = 0;
        for my $Menu ( sort keys %Menus ) {

            # load module
            if ( $Kernel::OM->Get('Kernel::System::Main')->Require( $Menus{$Menu}->{Module} ) ) {
                my $Object = $Menus{$Menu}->{Module}->new(
                    %{$Self},
                    ServiceID => $Self->{ServiceID},
                );

                # set classes
                if ( $Menus{$Menu}->{Target} ) {
                    if ( $Menus{$Menu}->{Target} eq 'PopUp' ) {
                        $Menus{$Menu}->{MenuClass} = 'AsPopup';
                    }
                    elsif ( $Menus{$Menu}->{Target} eq 'Back' ) {
                        $Menus{$Menu}->{MenuClass} = 'HistoryBack';
                    }
                }

                # run module
                $Counter = $Object->Run(
                    %Param,
                    Service => \%Service,
                    Counter => $Counter,
                    Config  => $Menus{$Menu},
                );
            }
            else {
                return $LayoutObject->FatalError();
            }
        }
    }

    # get sla object
    my $SLAObject = $Kernel::OM->Get('Kernel::System::SLA');

    # get sla list
    my %SLAList = $SLAObject->SLAList(
        ServiceID => $ServiceID,
        UserID    => $Self->{UserID},
    );
    if (%SLAList) {

        # output row
        $LayoutObject->Block(
            Name => 'SLA',
        );

        for my $SLAID ( sort { $SLAList{$a} cmp $SLAList{$b} } keys %SLAList ) {

            # get sla data
            my %SLA = $SLAObject->SLAGet(
                SLAID  => $SLAID,
                UserID => $Self->{UserID},
            );

            # output row
            $LayoutObject->Block(
                Name => 'SLARow',
                Data => {
                    %SLA,
                },
            );
        }
    }

    # get linked objects
    my $LinkListWithData = $Kernel::OM->Get('Kernel::System::LinkObject')->LinkListWithData(
        Object => 'Service',
        Key    => $ServiceID,
        State  => 'Valid',
        UserID => $Self->{UserID},
    );

    # get link table view mode
    my $LinkTableViewMode = $ConfigObject->Get('LinkObject::ViewMode');

    # create the link table
    my $LinkTableStrg = $LayoutObject->LinkObjectTableCreate(
        LinkListWithData => $LinkListWithData,
        ViewMode         => $LinkTableViewMode,
        Object           => 'Service',
        Key              => $ServiceID,
    );

    # output the link table
    if ($LinkTableStrg) {
        $LayoutObject->Block(
            Name => 'LinkTable' . $LinkTableViewMode,
            Data => {
                LinkTableStrg => $LinkTableStrg,
            },
        );
    }

    # set incident signal
    my %InciSignals = (
        operational => 'greenled',
        warning     => 'yellowled',
        incident    => 'redled',
    );

    # get user object
    my $UserObject = $Kernel::OM->Get('Kernel::System::User');

    # get create user data
    $Service{CreateByName} = $UserObject->UserName(
        UserID => $Service{CreateBy},
    );

    # get change user data
    $Service{ChangeByName} = $UserObject->UserName(
        UserID => $Service{ChangeBy},
    );

###### OTRSCMDBExplorer ######
    my $Tracer       = $Kernel::OM->Get('Kernel::System::CMDBExplorer');
    my $ParamObject  = $Kernel::OM->Get('Kernel::System::Web::Request');
    my $LinkObject   = $Kernel::OM->Get('Kernel::System::LinkObject');
    my $ConfigObject = $Kernel::OM->Get('Kernel::Config');

    # Get graph parameters from URI
    my %TraceParams;

#    my @DisplayedCIs;
#    if ( $ParamObject->GetParam( Param => 'DisplayedCIs' ) ) {
#        @DisplayedCIs = split( ',' ,$ParamObject->GetParam( Param => 'DisplayedCIs' ) );
#    }
#    else {
#        @DisplayedCIs = @RootCI;
#    }
#    $TraceParams{DisplayedCIs} =  \@DisplayedCIs;

    $TraceParams{ServiceID} = $ServiceID;
    $TraceParams{Layout} = $ParamObject->GetParam( Param => 'Layout' ) || 'dot';
    $TraceParams{IA}     = $ParamObject->GetParam( Param => 'IA' )     || 0;
    $TraceParams{Depth}  = $ParamObject->GetParam( Param => 'Depth' )  || 1;

    # Default trace constraints (Show all links up to specified depth)
    my @LinkTypes;
    my %TypeList = $LinkObject->TypeList( UserID => 1 );

    for ( keys %TypeList ) {
        push @LinkTypes, $_;
    }

    my $MaxTraceDepth = $TraceParams{Depth};

    # Does the agent requested Impact analysis?
    if ( $TraceParams{IA} == 1 ) {
        @LinkTypes = keys %{$ConfigObject->Get('ITSM::Core::IncidentLinkTypeDirection') || {} };
        $MaxTraceDepth = 0;
    }

    # Set Trace constraints
    $Tracer->SetConstraints(
        MaxTraceDepth => $MaxTraceDepth,
        LinkTypes => \@LinkTypes,
    );

    # Display Impact analysis menu entry
    if ( $Service{CurInciStateType} ne 'operational' ) {
        if ( ! $TraceParams{IA} ) {
            $LayoutObject->Block(
                Name => 'GraphMenuItem',
                Data => {
                    Link        => 'Action=AgentITSMServiceZoom;ServiceID=' . $ServiceID . ';Layout=' . $TraceParams{Layout} . ';IA=1',
                    MenuClass   => 'NoPopUp',
                    Name        => 'Impact',
                    Description => 'Show all impacted CIs',
                },
            );
        }
        else {
            $LayoutObject->Block(
                Name => 'GraphMenuItem',
                Data => {
                    Link        => 'Action=AgentITSMServiceZoom;ServiceID=' . $ServiceID . ';Layout=' . $TraceParams{Layout} . ';IA=0',
                    MenuClass   => 'NoPopUp',
                    Name        => 'Regular',
                    Description => 'Regular view',
                },
            );
        }
    }

    # Display digging menu entries
    $TraceParams{Depth}++;

    $LayoutObject->Block(
        Name => 'GraphMenuItem',
        Data => {
            Link        => 'Action=AgentITSMServiceZoom;ServiceID=' . $ServiceID . ';Layout=' . $TraceParams{Layout} . ';IA=' . $TraceParams{IA} . ';Depth=' . $TraceParams{Depth},
            MenuClass   => 'NoPopUp',
            Name        => '+',
            Description => 'Drill down in CMDB',
        },
    );

    $TraceParams{Depth} -= 2;
    $TraceParams{Depth} = 1 unless $TraceParams{Depth};

    $LayoutObject->Block(
        Name => 'GraphMenuItem',
        Data => {
            Link        => 'Action=AgentITSMServiceZoom;ServiceID=' . $ServiceID . ';Layout=' . $TraceParams{Layout} . ';IA=' . $TraceParams{IA} . ';Depth=' . $TraceParams{Depth},
            MenuClass   => 'NoPopUp',
            Name        => '-',
            Description => 'Drill up in CMDB',
        },
    );


    # Display graph layout selection field
    my $LayoutStrg = $LayoutObject->BuildSelection(
        Name => 'Layout',
        Data => {
            dot   => 'dot',
            neato => 'neato',
            twopi => 'twopi',
            circo => 'circo',
            fdp   => 'fdp',
            sfdp  => 'sfdp',
        },
        SelectedValue => $TraceParams{Layout},
        Class         => 'Modernize',
    );

    $LayoutObject->Block(
         Name => 'GraphLayout',
         Data => {
             ServiceID    => $ServiceID,
             DisplayedCIs => $ParamObject->GetParam( Param => 'DisplayedCIs' ) || '',
             IA           => $TraceParams{IA},
             Depth        => $MaxTraceDepth,
             LayoutStrg   => $LayoutStrg,
         },
    );

    # Create the graph
    my $Content = $Tracer->Trace( %TraceParams );

    # Add graph to output
    my $Base64EncodedPNG = encode_base64( $Content->{png} );
    $LayoutObject->Block(
        Name => 'LinkGraph',
        Data => {
            Content => $Base64EncodedPNG,
            Map     => $Content->{map},
        },
    );
###### OTRSCMDBExplorer ######

    # store last screen
    $Kernel::OM->Get('Kernel::System::AuthSession')->UpdateSessionID(
        SessionID => $Self->{SessionID},
        Key       => 'LastScreenView',
        Value     => $Self->{RequestedURL},
    );

    # output header
    my $Output = $LayoutObject->Header();
    $Output .= $LayoutObject->NavigationBar();

    # generate output
    $Output .= $LayoutObject->Output(
        TemplateFile => 'AgentITSMServiceZoom',
        Data         => {
            %Param,
            %Service,
            CurInciSignal => $InciSignals{ $Service{CurInciStateType} },
        },
    );
    $Output .= $LayoutObject->Footer();

    return $Output;
}

1;
