{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvObjPickerComp.PAS, released on 2002-06-24.

The Initial Developer of the Original Code is  Marcel van Brakel [brakelm att chello dott nl]
Portions created by Marcel van Brakel are Copyright (C) 2002 Marcel van Brakel.
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
   TODO OWNER
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvQObjectPickerDialog;

interface

uses
  Windows, ActiveX, ComObj, SysUtils, Classes, 
  ObjSel,
  JvQBaseDlg, JvQTypes;

// (rom) Jv the type names?
type
  // indicates the type of scope
  TScopeType = (
    stTargetComputer,
    stUpLevelJoinedDomain, // an uplevel domain joined by the target computer
    stDownLevelJoinedDomain, // a downlevel domain joined by the target computer
    stEnterpriseDomain, // all Windows 2000 domains of which the target computer is a member
    stGlobalCatalog, // all domains in the enterprise
    stExternalUpLevelDomain, // all trusted, uplevel domains external to the enterprise
    stExternalDownLevelDomain, // all trusted, downlevel domains external to the enterprise
    stWorkGroup, // a workgroup joined by the target computer
    stUserEnteredUpLevelScope, // enables the user to enter an up level scope
    stUserEnteredDownLevelScope); // enables the user to enter a down level scope
  TScopeTypes = set of TScopeType;

  TScopeFlag = (
    sfStartingScope, // scope should be initially selected (only one scope can have this flag set)
    sfProviderWinNT, // ADSPath is converted to use the WinNT provider
    sfProviderLDAP, // ADSPath is converted to use the LDAP provider
    sfProviderGC, // ADSPath is converted to use the GC provider
    sfSidPath, // ADSPath with an objectSID attribute are converted to the form LDAP://<SID=x>
    sfDownLevelBuiltInPath); // If not specified, ADSPath for downlevel, well-known objects are empty
  TScopeFlags = set of TScopeFlag;

  // up level filter flags. if a flag is set, the object picker includes the specified object when the scope is
  // selected. e.g. if ulUsers is included, users are displayed..

  TUpLevelFilter = (
    ulIncludeAdvancedView,
    ulUsers,
    ulBuiltInGroups,
    ulWellKnownPrincipals,
    ulUniversalDistributionListGroups,
    ulUniversalSecurityGroups,
    ulGlobalDistributionListGroups,
    ulGlobalSecurityGroups,
    ulDomainLocalDistributionListGroups,
    ulDomainLocalSecurityGroups,
    ulContacts,
    ulComputers);
  TUpLevelFilters = set of TUpLevelFilter;

  // down level filter flags. if a flag is set, the object picker includes the specified object when the scope is
  // selected. e.g. if ulUsers is included, users are displayed..

  TDownLevelFilter = (
    dlUsers,
    dlLocalGroups,
    dlGlobalGroups,
    dlComputers,
    dlWorld,
    dlAuthenticatedUser,
    dlAnonymous,
    dlBatch,
    dlCreatorOwner,
    dlCreatorGroup,
    dlDialUp,
    dlInteractive,
    dlNetwork,
    dlService,
    dlSystem,
    dlExcludeBuiltinGroups,
    dlTerminalServer,
    dlAllWellKnownSids,
    dlLocalService,
    dlNetworkService,
    dlRemoteLogon);
  TDownLevelFilters = set of TDownLevelFilter;

  // represents a single scope and it's associated filter

  TObjectPickerScope = class(TCollectionItem)
  private
    FDownLevelFilter: TDownLevelFilters;
    FDcName: string;
    FResult: HRESULT;
    FScopeTypes: TScopeTypes;
    FScopeFlags: TScopeFlags;
    FUpLevelFilterBoth: TUpLevelFilters;
    FUpLevelFilterNative: TUpLevelFilters;
    FUpLevelFilterMixed: TUpLevelFilters;
  public
    procedure Assign(Source: TPersistent); override;
  published
    // filter flags for down level scopes
    property DownLevelFilter: TDownLevelFilters read FDownLevelFilter write
      FDownLevelFilter default [];
    // name of a domain controller of the domain which the target computer is a member of, can be empty
    property DcName: string read FDcName write FDcName;
    // indicates whether this scope was succesfully initialized
    property Result: HRESULT read FResult default S_OK;
    // the type of scope (e.g. enterprise domain, global catalog or computer)
    property ScopeTypes: TScopeTypes read FScopeTypes write FScopeTypes default [];
    // flags  that indicate the format of the returned ADSPath and whether this scope should be initially selected
    property ScopeFlags: TScopeFlags read FScopeFlags write FScopeFlags default [];
    // filter flags for up level scope in either mode (native or mixed)
    property UpLevelFilterBoth: TUpLevelFilters read FUpLevelFilterBoth write
      FUpLevelFilterBoth default [];
    // filter flags for up level scope in native mode
    property UpLevelFilterNative: TUpLevelFilters read FUpLevelFilterNative write
      FUpLevelFilterNative default [];
    // filter flags for up level scope in mixed mode
    property UpLevelFilterMixed: TUpLevelFilters read FUpLevelFilterMixed write
      FUpLevelFilterMixed default [];
  end;

  // list of scopes

  TObjectPickerScopes = class(TCollection)
  private
    //OWNER FOwner: TComponent;
    function GetItem(Index: Integer): TObjectPickerScope;
    procedure SetItem(Index: Integer; Value: TObjectPickerScope);
  protected
    procedure Initialize(var ScopesInitInfo: array of TDsOpScopeInitInfo);
  public
    constructor Create({OWNER AOwner: TComponent});
    // adds a scope
    function Add: TObjectPickerScope;
    // assigns a scope
    procedure Assign(Source: TPersistent); override;
    // the owner of this class
    //OWNER function Owner: TComponent;
    // list of scopes
    property Items[Index: Integer]: TObjectPickerScope read GetItem write SetItem; default;
  end;

  TObjectPickerSelection = class(TObject)
  private
    FAttributeCount: Integer;
    FSelection: PDsSelection;
    function GetAttribute(Index: Integer): OleVariant;
    function GetAdsPath: string;
    function GetName: string;
    function GetObjectClass: string;
    function GetScopeTypes: TScopeTypes;
    function GetUPN: string;
  public
    constructor Create(Selection: PDsSelection; const AttributeCount: Integer);
    // the Relative Distinquishged Name (RDN) of the object
    property Name: string read GetName;
    // the object's ADSPath. format depends on what flags you specified for the scope the object was selected from
    property AdsPath: string read GetAdsPath;
    // the class of the object (objectClass attribute)
    property ObjectClass: string read GetObjectClass;
    // the object's userPrincipalName attribute, or an empty string if it doesn't have a UPN attribute
    property UPN: string read GetUPN;
    // specifies the scope the object was selected from
    property ScopeType: TScopeTypes read GetScopeTypes;
    // the number of entrie sin the Attributes list
    property AttributeCount: Integer read FAttributeCount;
    // list of attribute values, one for each requested attribute (in the same order as requested). if an attribute
    // couldn't be retrieved the element contains an empty variant (use f.e. VarIsEmpty to test).
    property Attributes[Index: Integer]: OleVariant read GetAttribute;
  end;

  // class encapsulating the selection list. it contains the list of selected objects

  TObjectPickerSelections = class(TObject)
  private
    FItems: array of TObjectPickerSelection;
    FMedium: TStgMedium;
    FSelections: PDsSelectionList;
    function GetAttributeCount: Integer;
    function GetItem(Index: Integer): TObjectPickerSelection;
    function GetCount: Integer;
  protected
    procedure FreeSelection;
    procedure SetSelection(const DataObj: IDataObject);
  public
    destructor Destroy; override;
    // the number of attributes retrieved for each selected object (also avail. as TObjectPickerSelection.AttributeCount)
    property AttributeCount: Integer read GetAttributeCount;
    // the number of entries in the Items list
    property Count: Integer read GetCount;
    // list of objects, each represents a single selected object
    property Items[Index: Integer]: TObjectPickerSelection read GetItem;
    default;
  end;

  // Global Object Picker options

  TObjectPickerOption = (opAllowMultiSelect, // allow selection of multiple objects
    opSkipTargetComputerDCCheck); // skip DC check if target computer is a domain controller
  TObjectPickerOptions = set of TObjectPickerOption;

  // the Object Picker dialog component

  TJvObjectPickerDialog = class(TJvCommonDialog)
  private
    FAttributes: TStringList;
    FObjectPicker: IDsObjectPicker;
    FOptions: TObjectPickerOptions;
    FScopes: TObjectPickerScopes;
    FSelection: TObjectPickerSelections;
    FTargetComputer: string;
    function GetAttributes: TStrings;
    procedure SetAttributes(Value: TStrings);
    procedure SetScopes(Value: TObjectPickerScopes);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // executes (displays) the object picker dialog
    function Execute: Boolean; override;
    // resets the object picker. clears all options, scopes and attributes
    procedure Reset;
    // the list of selected objects is available through this class
    property Selection: TObjectPickerSelections read FSelection;
  published
    // list of additional attributes the Object Picker should retrieve for all selected objects
    property Attributes: TStrings read GetAttributes write SetAttributes;
    // global options, see TObjectPickerOptions
    property Options: TObjectPickerOptions read FOptions write FOptions default [];
    // the available scopes and their filters
    property Scopes: TObjectPickerScopes read FScopes write SetScopes;
    // the target computer the Object Picker uses to determine the joined domain and enterprise. the Object Picker
    // behaves as if it's running on the specified computer. leave empty for the local computer.
    property TargetComputer: string read FTargetComputer write FTargetComputer;
  end;

  // object picker exception class
  // just to be able to distinguish between exceptions raised by the Object Picker specifically and all others
  EObjectPickerError = class(EJVCLException);

implementation

uses
  JvQResources;

function ScopeTypesToOrdinal(const ScopeTypes: TScopeTypes): Cardinal;
begin
  Result := 0;
  if stTargetComputer in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_TARGET_COMPUTER;
  if stUpLevelJoinedDomain in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_UPLEVEL_JOINED_DOMAIN;
  if stDownLevelJoinedDomain in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_DOWNLEVEL_JOINED_DOMAIN;
  if stEnterpriseDomain in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_ENTERPRISE_DOMAIN;
  if stGlobalCatalog in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_GLOBAL_CATALOG;
  if stExternalUpLevelDomain in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_EXTERNAL_UPLEVEL_DOMAIN;
  if stExternalDownLevelDomain in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_EXTERNAL_DOWNLEVEL_DOMAIN;
  if stWorkGroup in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_WORKGROUP;
  if stUserEnteredUpLevelScope in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_USER_ENTERED_UPLEVEL_SCOPE;
  if stUserEnteredDownLevelScope in ScopeTypes then
    Result := Result or DSOP_SCOPE_TYPE_USER_ENTERED_DOWNLEVEL_SCOPE;
end;

function OrdinalToScopeTypes(const Ordinal: Cardinal): TScopeTypes;
begin
  Result := [];
  if (Ordinal and DSOP_SCOPE_TYPE_TARGET_COMPUTER) <> 0 then
    Include(Result, stTargetComputer);
  if (Ordinal and DSOP_SCOPE_TYPE_UPLEVEL_JOINED_DOMAIN) <> 0 then
    Include(Result, stUpLevelJoinedDomain);
  if (Ordinal and DSOP_SCOPE_TYPE_DOWNLEVEL_JOINED_DOMAIN) <> 0 then
    Include(Result, stDownLevelJoinedDomain);
  if (Ordinal and DSOP_SCOPE_TYPE_ENTERPRISE_DOMAIN) <> 0 then
    Include(Result, stEnterpriseDomain);
  if (Ordinal and DSOP_SCOPE_TYPE_GLOBAL_CATALOG) <> 0 then
    Include(Result, stGlobalCatalog);
  if (Ordinal and DSOP_SCOPE_TYPE_EXTERNAL_UPLEVEL_DOMAIN) <> 0 then
    Include(Result, stExternalUpLevelDomain);
  if (Ordinal and DSOP_SCOPE_TYPE_EXTERNAL_DOWNLEVEL_DOMAIN) <> 0 then
    Include(Result, stExternalDownLevelDomain);
  if (Ordinal and DSOP_SCOPE_TYPE_WORKGROUP) <> 0 then
    Include(Result, stWorkGroup);
  if (Ordinal and DSOP_SCOPE_TYPE_USER_ENTERED_UPLEVEL_SCOPE) <> 0 then
    Include(Result, stUserEnteredUpLevelScope);
  if (Ordinal and DSOP_SCOPE_TYPE_USER_ENTERED_DOWNLEVEL_SCOPE) <> 0 then
    Include(Result, stUserEnteredDownLevelScope);
end;

function ScopeFlagsToOrdinal(const ScopeFlags: TScopeFlags): Cardinal;
begin
  Result := 0;
  if sfStartingScope in ScopeFlags then
    Result := Result or DSOP_SCOPE_FLAG_STARTING_SCOPE;
  if sfProviderWinNT in ScopeFlags then
    Result := Result or DSOP_SCOPE_FLAG_WANT_PROVIDER_WINNT;
  if sfProviderLDAP in ScopeFlags then
    Result := Result or DSOP_SCOPE_FLAG_WANT_PROVIDER_LDAP;
  if sfProviderGC in ScopeFlags then
    Result := Result or DSOP_SCOPE_FLAG_WANT_PROVIDER_GC;
  if sfSidPath in ScopeFlags then
    Result := Result or DSOP_SCOPE_FLAG_WANT_SID_PATH;
  if sfDownLevelBuiltInPath in ScopeFlags then
    Result := Result or DSOP_SCOPE_FLAG_WANT_DOWNLEVEL_BUILTIN_PATH;
  //DSOP_SCOPE_FLAG_DEFAULT_FILTER_USERS        = $00000040;
  //DSOP_SCOPE_FLAG_DEFAULT_FILTER_GROUPS       = $00000080;
  //DSOP_SCOPE_FLAG_DEFAULT_FILTER_COMPUTERS    = $00000100;
  //DSOP_SCOPE_FLAG_DEFAULT_FILTER_CONTACTS     = $00000200;
end;

function UpLevelFilterToOrdinal(const Filter: TUpLevelFilters): Cardinal;
begin
  Result := 0;
  if ulIncludeAdvancedView in Filter then
    Result := Result or DSOP_FILTER_INCLUDE_ADVANCED_VIEW;
  if ulUsers in Filter then
    Result := Result or DSOP_FILTER_USERS;
  if ulBuiltInGroups in Filter then
    Result := Result or DSOP_FILTER_BUILTIN_GROUPS;
  if ulWellKnownPrincipals in Filter then
    Result := Result or DSOP_FILTER_WELL_KNOWN_PRINCIPALS;
  if ulUniversalDistributionListGroups in Filter then
    Result := Result or DSOP_FILTER_UNIVERSAL_GROUPS_DL;
  if ulUniversalSecurityGroups in Filter then
    Result := Result or DSOP_FILTER_UNIVERSAL_GROUPS_SE;
  if ulGlobalDistributionListGroups in Filter then
    Result := Result or DSOP_FILTER_GLOBAL_GROUPS_DL;
  if ulGlobalSecurityGroups in Filter then
    Result := Result or DSOP_FILTER_GLOBAL_GROUPS_SE;
  if ulDomainLocalDistributionListGroups in Filter then
    Result := Result or DSOP_FILTER_DOMAIN_LOCAL_GROUPS_DL;
  if ulDomainLocalSecurityGroups in Filter then
    Result := Result or DSOP_FILTER_DOMAIN_LOCAL_GROUPS_SE;
  if ulContacts in Filter then
    Result := Result or DSOP_FILTER_CONTACTS;
  if ulComputers in Filter then
    Result := Result or DSOP_FILTER_COMPUTERS;
end;

function DownLevelFilterToOrdinal(const Filter: TDownLevelFilters): Cardinal;
begin
  Result := 0;
  if dlUsers in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_USERS;
  if dlLocalGroups in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_LOCAL_GROUPS;
  if dlGlobalGroups in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_GLOBAL_GROUPS;
  if dlComputers in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_COMPUTERS;
  if dlWorld in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_WORLD;
  if dlAuthenticatedUser in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_AUTHENTICATED_USER;
  if dlAnonymous in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_ANONYMOUS;
  if dlBatch in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_BATCH;
  if dlCreatorOwner in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_CREATOR_OWNER;
  if dlCreatorGroup in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_CREATOR_GROUP;
  if dlDialUp in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_DIALUP;
  if dlInteractive in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_INTERACTIVE;
  if dlNetwork in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_NETWORK;
  if dlService in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_SERVICE;
  if dlSystem in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_SYSTEM;
  if dlExcludeBuiltinGroups in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_EXCLUDE_BUILTIN_GROUPS;
  if dlTerminalServer in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_TERMINAL_SERVER;
  if dlAllWellKnownSids in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_ALL_WELLKNOWN_SIDS;
  if dlLocalService in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_LOCAL_SERVICE;
  if dlNetworkService in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_NETWORK_SERVICE;
  if dlRemoteLogon in Filter then
    Result := Result or DSOP_DOWNLEVEL_FILTER_REMOTE_LOGON;
end;

function OptionsToOrdinal(const Options: TObjectPickerOptions): Cardinal;
begin
  Result := 0;
  if opAllowMultiSelect in Options then
    Result := Result or DSOP_FLAG_MULTISELECT;
  if opSkipTargetComputerDCCheck in Options then
    Result := Result or DSOP_FLAG_SKIP_TARGET_COMPUTER_DC_CHECK;
end;

//=== { TObjectPickerScope } =================================================

procedure TObjectPickerScope.Assign(Source: TPersistent);
begin
  if Source is TObjectPickerScope then
  begin
    FDownLevelFilter := TObjectPickerScope(Source).DownLevelFilter;
    FScopeTypes := TObjectPickerScope(Source).ScopeTypes;
    FScopeFlags := TObjectPickerScope(Source).ScopeFlags;
    FDcName := TObjectPickerScope(Source).DcName;
    FResult := TObjectPickerScope(Source).Result;
    FUpLevelFilterBoth := TObjectPickerScope(Source).UpLevelFilterBoth;
    FUpLevelFilterNative := TObjectPickerScope(Source).UpLevelFilterNative;
    FUpLevelFilterMixed := TObjectPickerScope(Source).UpLevelFilterMixed;
  end
  else
    inherited Assign(Source);
end;

//=== { TObjectPickerScopes } ================================================

constructor TObjectPickerScopes.Create({OWNER AOwner: TComponent});
begin
  inherited Create(TObjectPickerScope);
  //OWNER FOwner := AOwner;
end;

function TObjectPickerScopes.Add: TObjectPickerScope;
begin
  Result := TObjectPickerScope(inherited Add);
end;

procedure TObjectPickerScopes.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TObjectPickerScopes then
    for I := 0 to TCollection(Source).Count - 1 do
      Add.Assign(TCollection(Source).Items[I])
  else
    inherited Assign(Source);
end;

function TObjectPickerScopes.GetItem(Index: Integer): TObjectPickerScope;
begin
  Result := TObjectPickerScope(inherited Items[Index]);
end;

procedure TObjectPickerScopes.Initialize(var ScopesInitInfo: array of TDsOpScopeInitInfo);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    FillChar(ScopesInitInfo[I], SizeOf(TDsOpScopeInitInfo), 0);
    ScopesInitInfo[I].cbSize := SizeOf(TDsOpScopeInitInfo);
    ScopesInitInfo[I].flType := ScopeTypesToOrdinal(Items[I].ScopeTypes);
    ScopesInitInfo[I].flScope := ScopeFlagsToOrdinal(Items[I].ScopeFlags);
    ScopesInitInfo[I].FilterFlags.Uplevel.flBothModes :=
      UpLevelFilterToOrdinal(Items[I].UpLevelFilterBoth);
    ScopesInitInfo[I].FilterFlags.Uplevel.flMixedModeOnly :=
      UpLevelFilterToOrdinal(Items[I].UpLevelFilterMixed);
    ScopesInitInfo[I].FilterFlags.Uplevel.flNativeModeOnly :=
      UpLevelFilterToOrdinal(Items[I].UpLevelFilterNative);
    ScopesInitInfo[I].FilterFlags.flDownlevel :=
      DownLevelFilterToOrdinal(Items[I].DownLevelFilter);
    ScopesInitInfo[I].pwzDcName := PWideChar(WideString(Items[I].DcName));
    ScopesInitInfo[I].pwzADsPath := nil;
    ScopesInitInfo[I].hr := S_OK;
  end;
end;

//OWNER function TObjectPickerScopes.Owner: TComponent;
//OWNER begin
//OWNER   Result := FOwner;
//OWNER end;

procedure TObjectPickerScopes.SetItem(Index: Integer;
  Value: TObjectPickerScope);
begin
  TObjectPickerScope(inherited Items[Index]).Assign(Value);
end;

//=== { TObjectPickerSelection } =============================================

constructor TObjectPickerSelection.Create(Selection: PDsSelection;
  const AttributeCount: Integer);
begin
  inherited Create;
  FAttributeCount := AttributeCount;
  FSelection := Selection;
end;

function TObjectPickerSelection.GetAdsPath: string;
begin
  Result := WideCharToString(FSelection^.pwzADsPath);
end;

function TObjectPickerSelection.GetAttribute(Index: Integer): OleVariant;
type
  TOleVariantArray = array [0..(MaxInt div SizeOf(OleVariant)) - 1] of OleVariant;
  POleVariantArray = ^TOleVariantArray;
begin
  if (Index < 0) or (Index >= AttributeCount) then
    raise EObjectPickerError.CreateResFmt(@RsEAttributeIndexOutOfBounds, [Index]);
  Result := POleVariantArray(FSelection^.pvarFetchedAttributes)^[Index];
end;

function TObjectPickerSelection.GetName: string;
begin
  Result := WideCharToString(FSelection^.pwzName);
end;

function TObjectPickerSelection.GetObjectClass: string;
begin
  Result := WideCharToString(FSelection^.pwzClass);
end;

function TObjectPickerSelection.GetScopeTypes: TScopeTypes;
begin
  Result := OrdinalToScopeTypes(FSelection^.flScopeType);
end;

function TObjectPickerSelection.GetUPN: string;
begin
  Result := WideCharToString(FSelection^.pwzUPN);
end;

//=== { TObjectPickerSelections } ============================================

destructor TObjectPickerSelections.Destroy;
begin
  FreeSelection;
  inherited Destroy;
end;

procedure TObjectPickerSelections.FreeSelection;
var
  I: Integer;
begin
  if FSelections <> nil then
  begin
    for I := 0 to Length(FItems) - 1 do
      FItems[I].Free;
    SetLength(FItems, 0);
    GlobalUnlock(FMedium.hGlobal);
    ReleaseStgMedium(FMedium);
    FSelections := nil;
  end;
end;

function TObjectPickerSelections.GetAttributeCount: Integer;
begin
  Result := -1;
  if FSelections <> nil then
    Result := FSelections^.cFetchedAttributes;
end;

function TObjectPickerSelections.GetCount: Integer;
begin
  Result := -1;
  if FSelections <> nil then
    Result := FSelections^.cItems;
end;

function TObjectPickerSelections.GetItem(Index: Integer): TObjectPickerSelection;
begin
  Result := nil;
  if FSelections <> nil then
  begin
    if (Index < 0) or (Index >= Count) then
      raise EObjectPickerError.CreateResFmt(@RsESelectionIndexOutOfBounds, [Index]);
    Result := FItems[Index];
  end;
end;

procedure TObjectPickerSelections.SetSelection(const DataObj: IDataObject);
var
  Format: TFormatEtc;
  I: Integer;
  Selection: PDsSelection;
  HRes: HRESULT;
begin
  FreeSelection;
  Format.cfFormat := RegisterClipboardFormat(CFSTR_DSOP_DS_SELECTION_LIST);
  Format.ptd := nil;
  Format.dwAspect := DVASPECT_CONTENT;
  Format.lindex := -1;
  Format.tymed := TYMED_HGLOBAL;
  FillChar(FMedium, SizeOf(FMedium), 0);
  FMedium.tymed := TYMED_HGLOBAL;
  HRes := DataObj.GetData(Format, FMedium);
  if Succeeded(HRes) then
  begin
    FSelections := GlobalLock(FMedium.hGlobal);
    SetLength(FItems, FSelections^.cItems);
    for I := 0 to FSelections^.cItems - 1 do
    begin
      {$RANGECHECKS OFF}
      Selection := @FSelections^.aDsSelection[I];
      {$IFDEF RANGECHECKS_ON}
      {$RANGECHECKS ON}
      {$ENDIF RANGECHECKS_ON}
      FItems[I] := TObjectPickerSelection.Create(Selection,
        FSelections^.cFetchedAttributes);
    end;
  end
  else
    OleCheck(HRes);
end;

//=== { TJvObjectPickerDialog } ==============================================

constructor TJvObjectPickerDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  OleCheck(CoCreateInstance(CLSID_DsObjectPicker, nil, CLSCTX_INPROC_SERVER, IID_IDsObjectPicker, FObjectPicker));
  FAttributes := TStringList.Create;
  FOptions := [];
  FScopes := TObjectPickerScopes.Create({OWNER Self});
  FSelection := TObjectPickerSelections.Create;
  FTargetComputer := '';
end;

destructor TJvObjectPickerDialog.Destroy;
begin
  FSelection.Free;
  FScopes.Free;
  FAttributes.Free;
  FObjectPicker := nil;
  inherited Destroy;
end;

function TJvObjectPickerDialog.Execute: Boolean;
var
  InitInfo: TDsOpInitInfo;
  ScopesInitInfo: array of TDsOpScopeInitInfo;
  Attrs: array of PWideChar;
  AttrStrs: array of WideString;
  HRes: HRESULT;
  DataObj: IDataObject;

  procedure InitializeAttributes;
  var
    I: Integer;
  begin
    InitInfo.cAttributesToFetch := Attributes.Count;
    if Attributes.Count = 0 then
      InitInfo.apwzAttributeNames := nil
    else
    begin
      SetLength(Attrs, Attributes.Count);
      SetLength(AttrStrs, Attributes.Count);
      for I := 0 to Attributes.Count - 1 do
      begin
        AttrStrs[I] := WideString(Attributes[I]);
        Attrs[I] := PWideChar(AttrStrs[I]);
      end;
      InitInfo.apwzAttributeNames := @Attrs[0];
    end;
  end;

  procedure PropogateInitResults;
  var
    I: Integer;
  begin
    for I := 0 to Scopes.Count - 1 do
      Scopes[I].FResult := ScopesInitInfo[I].hr;
  end;

begin
  Result := False;
  OleCheck(CoCreateInstance(CLSID_DsObjectPicker, nil, CLSCTX_INPROC_SERVER,
    IID_IDsObjectPicker, FObjectPicker));
  FillChar(InitInfo, SizeOf(InitInfo), 0);
  InitInfo.cbSize := SizeOf(InitInfo);
  InitInfo.flOptions := OptionsToOrdinal(FOptions);
  InitInfo.cDsScopeInfos := Scopes.Count;
  SetLength(ScopesInitInfo, Scopes.Count);
  InitInfo.aDsScopeInfos := @ScopesInitInfo[0];
  Scopes.Initialize(ScopesInitInfo);
  InitializeAttributes;
  Selection.FreeSelection;
  HRes := FObjectPicker.Initialize(InitInfo);
  // (p3) this won't raise a second exception
  if not Succeeded(HRes) then
    Exit;
  PropogateInitResults;
//  OleCheck(HRes);
  HRes := FObjectPicker.InvokeDialog(0, DataObj);
  case HRes of
    S_OK:
      begin
        Result := True;
        FSelection.SetSelection(DataObj);
      end;
    S_FALSE:
      Result := False;
  else
    Result := False;
    OleCheck(HRes);
  end;
end;

procedure TJvObjectPickerDialog.Reset;
begin
  Attributes.Clear;
  Options := [];
  Scopes.Clear;
  Selection.FreeSelection;
end;

function TJvObjectPickerDialog.GetAttributes: TStrings;
begin
  Result := FAttributes;
end;

procedure TJvObjectPickerDialog.SetAttributes(Value: TStrings);
begin
  FAttributes.Assign(Value);
end;

procedure TJvObjectPickerDialog.SetScopes(Value: TObjectPickerScopes);
begin
  FScopes.Assign(Value);
end;

end.

