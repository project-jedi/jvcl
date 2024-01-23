unit DtxPasCombiner;

interface

uses
  Classes,
  DtxParser, JVCLHelpUtils, DelphiParser, ParserTypes;

type
  TLinkedList = class
  private
    FLeftIndexen: TIntegerList;
    FRightIndexen: TIntegerList;
    FLeftUnlinkedCount: Integer;
    FRightUnlinkedCount: Integer;
    function GetIsLeftLinked(const ALeftIndex: Integer): Boolean;
    function GetIsRightLinked(const ARightIndex: Integer): Boolean;
    function GetLeftCount: Integer;
    function GetLeftLinkedWith(const ALeftIndex: Integer): Integer;
    function GetRightCount: Integer;
    function GetRightLinkedWith(const ARightIndex: Integer): Integer;
    procedure SetLeftCount(const Value: Integer);
    procedure SetLeftLinkedWith(const ALeftIndex, Value: Integer);
    procedure SetRightCount(const Value: Integer);
    procedure SetRightLinkedWith(const ARightIndex, Value: Integer);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Reset;
    procedure DeleteLeft(const ALeftIndex: Integer);
    procedure DeleteRight(const ARightIndex: Integer);
    function UnLink(const ALeftIndex, ARightIndex: Integer): Boolean;
    function Link(const ALeftIndex, ARightIndex: Integer): Boolean;
    property LeftLinkedWith[const ALeftIndex: Integer]: Integer read GetLeftLinkedWith write SetLeftLinkedWith;
    property RightLinkedWith[const ARightIndex: Integer]: Integer read GetRightLinkedWith write SetRightLinkedWith;
    property IsLeftLinked[const ALeftIndex: Integer]: Boolean read GetIsLeftLinked;
    property IsRightLinked[const ARightIndex: Integer]: Boolean read GetIsRightLinked;
    property LeftCount: Integer read GetLeftCount write SetLeftCount;
    property RightCount: Integer read GetRightCount write SetRightCount;
    property LeftUnlinkedCount: Integer read FLeftUnlinkedCount;
    property RightUnlinkedCount: Integer read FRightUnlinkedCount;
  end;

  TDtxPasCombiner = class
  private
    FDestXMLFileName: string;
    FSourcePasFileName: string;
    //    FSourceDtxFileName: string;
    FDtxList: TDtxList;
    FPasParser: TDelphiParser;
    FLinkedList: TLinkedList;
    FDestDtxFileName: string;
    FRegisteredComponents: TRegisteredComponents;
    FOnlyCheck: Boolean;
    FPercComplete: Integer;
    FGenerateForBuild: Boolean;
    FCopyHeaderFromOriginal: Boolean;
    FOriginalDtxFileName: string;
    FUpdateOriginalEmpties: Boolean;
    FCopySeeAlsoFromOriginal: Boolean;
    FPackageList: TStrings;
    FNoSyncWithPas: Boolean;
    procedure ParseSeeAlsoText(List: TSymbolList; const SeeAlsoText: string);

    function FillLinkedList: Boolean;
    function FillInDefaultTitles: Boolean;
    function FillInDefaultsForGen: Boolean;
    function FillInDefaultsForBuild: Boolean;
    function ProcessSeeAlsosForBuild: Boolean;
    function ExpandPasDuplicates: Boolean;
    function RemovePasDuplicates: Boolean;
    function AddUndocumented: Boolean;
    function DoSyncWithOriginal: Boolean;
    function AddDefaultCombines: Boolean;
    function ExpandMissingDtxNames: Boolean;
    function AddExpandCombines: Boolean;
    function SetHASPASFILEENTRYForJCLTopics: Boolean;
    function CheckMissing: Boolean;

    function CheckPackage: Boolean;
    function CheckDuplicatesInDtx: Boolean;
    function CheckJVCLInfos: Boolean;
    function GroupListItem(APasItem: TListItem): Boolean;
    function GroupListItems: Boolean;
  protected

    procedure DeterminePercComplete;

    function Process: Boolean;
    function ProcessLinks: Boolean;
    function CombineList(APasItem: TListItem; ADtxTopic: TDtxTopic): Boolean;
    function CombineMethod(APasItem: TParamClassMethodItem; ADtxTopic: TDtxTopic): Boolean;
    function CombineFunction(APasItem: TBaseFuncItem; ADtxTopic: TDtxTopic): Boolean;
    function CombineItems(APasItem: TAbstractItem; ADtxTopic: TDtxTopic): Boolean;
    function CheckParams(APasParams: TStrings; ADtxTopic: TDtxTopic; const DoNotAdd: Boolean): Boolean;
    function CheckCombinedParams(APasParams: TStrings; ADtxTopic: TDtxTopic; const DoNotAdd: Boolean): Boolean;
    function CombineParams(APasParams: TStrings; ADtxTopic: TDtxTopic): Boolean;
    function SaveXML: Boolean;
    function SaveDtx: Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function Parse: Boolean;
    function Execute: Boolean;

    property OnlyCheck: Boolean read FOnlyCheck write FOnlyCheck;
    property GenerateForBuild: Boolean read FGenerateForBuild write FGenerateForBuild;
    property SourcePasFileName: string read FSourcePasFileName write FSourcePasFileName;
    property OriginalDtxFileName: string read FOriginalDtxFileName write FOriginalDtxFileName;
    //    property SourceDtxFileName: string read FSourceDtxFileName write FSourceDtxFileName;
    property DestXMLFileName: string read FDestXMLFileName write FDestXMLFileName;
    property DestDtxFileName: string read FDestDtxFileName write FDestDtxFileName;
    property RegisteredComponents: TRegisteredComponents read FRegisteredComponents write FRegisteredComponents;
    property PackageList: TStrings read FPackageList write FPackageList;

    property NoSyncWithPas: Boolean read FNoSyncWithPas write FNoSyncWithPas;
    property CopyHeaderFromOriginal: Boolean read FCopyHeaderFromOriginal write FCopyHeaderFromOriginal;
    property UpdateOriginalEmpties: Boolean read FUpdateOriginalEmpties write FUpdateOriginalEmpties;
    property CopySeeAlsoFromOriginal: Boolean read FCopySeeAlsoFromOriginal write FCopySeeAlsoFromOriginal;

    property DtxList: TDtxList read FDtxList write FDtxList;
    property PercComplete: Integer read FPercComplete;
  end;

implementation

uses
  SysUtils, Dialogs, JvDSADialogs;

const
  cDefaultAcceptCompilerDirectivesArray: array[0..32] of string = (
    {0}'ANIMATE',
    'COMPILER1_UP',
    'COMPILER2_UP',
    'COMPILER3_UP',
    'COMPILER35_UP',
    {5}'COMPILER4_UP',
    'COMPILER5_UP',
    'COMPILER6',
    'COMPILER6_UP',
    'COMPLIB_VCL',
    {10}'DELPHI',
    'DELPHI1_UP',
    'DELPHI2_UP',
    'DELPHI3_UP',
    'DELPHI4_UP',
    {15}'DELPHI5_UP',
    'DELPHI6',
    'DELPHI6_UP',
    'JV_MIDAS',
    'JVCLThemesEnabled',
    {20}'MSWINDOWS',
    'NeedMouseEnterLeave',
    'SUPPORTS_DEFAULTPARAMS',
    'SUPPORTS_DYNAMICARRAYS',
    'SUPPORTS_EXTSYM',
    {25}'SUPPORTS_INTERFACE',
    'SUPPORTS_OVERLOAD',
    'SUPPORTS_WIDESTRING',
    'TABINFO',
    'USE_JV_GIF',
    {30}'USEJVCL',
    'VCL',
    'WIN32'
    );

  cOptionalDelphiTypes = [dtConst, dtResourceString];

var
  GDefaultAcceptCompilerDirectives: TStrings;

  //=== Local procedures =======================================================

procedure FillDefaultAcceptCompilerDirectives(AStrings: TStrings);
var
  I: Integer;
begin
  with AStrings do
    for I := Low(cDefaultAcceptCompilerDirectivesArray) to High(cDefaultAcceptCompilerDirectivesArray) do
      AStrings.Add(cDefaultAcceptCompilerDirectivesArray[i]);
end;

function DefaultAcceptCompilerDirectives: TStrings;
begin
  if GDefaultAcceptCompilerDirectives = nil then
  begin
    GDefaultAcceptCompilerDirectives := TStringList.Create;
    FillDefaultAcceptCompilerDirectives(GDefaultAcceptCompilerDirectives);
  end;

  Result := GDefaultAcceptCompilerDirectives;
end;

function RightDotName(const S: string): string;
var
  DotPos: PChar;
begin
  DotPos := StrScan(PChar(S), '.');
  if Assigned(DotPos) then
  begin
    Inc(DotPos);
    SetString(Result, DotPos, PChar(S) + Length(S) - DotPos);
  end
  else
    Result := '';
end;

//=== { TDtxPasCombiner } ====================================================

constructor TDtxPasCombiner.Create;
begin
  inherited Create;
  //  FDtxList := TDtxList.Create;
  FPasParser := TDelphiParser.Create;
  FLinkedList := TLinkedList.Create;
end;

destructor TDtxPasCombiner.Destroy;
begin
  FLinkedList.Free;
  FPasParser.Free;
  //  FDtxList.Free;
  inherited Destroy;
end;

function TDtxPasCombiner.AddDefaultCombines: Boolean;
var
  I: Integer;
  ATopic: TDtxTopic;
  APasItem: TAbstractItem;
begin
  if NoSyncWithPas then
  begin
    Result := True;
    Exit;
  end;

  Result := True;
  for I := 0 to FDtxList.Count - 1 do
  begin
    ATopic := FDtxList.Topics[i];
    if (ATopic.CombineTopic = nil) and (ATopic.CombineWithCount = 0) and
      FLinkedList.IsLeftLinked[i] then
    begin
      APasItem := FPasParser.TypeList[FLinkedList.LeftLinkedWith[i]];
      if APasItem.CombineCount > 1 then
      begin
        { Single procedure type used for multiple properties }
      end
      else
      if APasItem.CombineCount > 0 then
      begin
        HintMsgFmt('Added <%s> as combine for %s',
          [APasItem.CombineString, ATopic.Name]);
        ATopic.Combine := APasItem.CombineString;
        Result := ATopic.ResolveCombines;
        if not Result then
          Exit;
      end
        //      else
        //        WarningMsgFmt('Unexpected COMBINE(s) in <%s>', [ATopic.Name]);
    end;
  end;
  for I := 0 to FPasParser.TypeList.Count - 1 do
  begin
    APasItem := FPasParser.TypeList[i];
    if not IsNullStr(APasItem.CombineString) and
      FLinkedList.IsRightLinked[i] then
    begin
      ATopic := FDtxList.Topics[FLinkedList.RightLinkedWith[i]];
      if ATopic.CombineTopic = nil then
        WarningMsgFmt('Not a COMBINE in <%s>, should be <%s>?',
          [ATopic.Name, APasITem.CombineString])
      else
        if not SameText(ATopic.CombineTopic.Name, APasItem.CombineString) then
        WarningMsgFmt('Other COMBINE in <%s>; act:<%s> exp:<%s>',
          [ATopic.Name, ATopic.CombineTopic.Name, APasItem.CombineString])
    end;
  end;
end;

function TDtxPasCombiner.AddExpandCombines: Boolean;
var
  PasIndex, PasIndex2: Integer;
  APasItem, APasItem2: TAbstractItem;
  ATopic, NewTopic: TDtxTopic;
begin
  if NoSyncWithPas then
  begin
    Result := True;
    Exit;
  end;

  Result := True;

  for PasIndex := 0 to FPasParser.TypeList.Count - 1 do
  begin
    APasItem := FPasParser.TypeList[PasIndex];
    // should it be documented?
//    if not (APasItem.DelphiType in cOptionalDelphiTypes) and
//      not APasItem.IsOptionalInDtx and
    if not FLinkedList.IsRightLinked[PasIndex] and
      not FDtxList.IsOnSkipList(APasItem.ReferenceName) then
    begin
      // is another pas item with the same documented; ie is a overloaded
      //  procedure/function etc. documented?
      for PasIndex2 := 0 to FPasParser.TypeList.Count - 1 do
        if PasIndex2 <> PasIndex then
        begin
          APasItem2 := FPasParser.TypeList[PasIndex2];

          // same name? and documented
          if SameText(APasItem.LinkName, APasItem2.LinkName) and
            FLinkedList.IsRightLinked[PasIndex2] then
          begin
            // check the dtx item
            ATopic := FDtxList[FLinkedList.RightLinkedWith[PasIndex2]];
            // is it documented?
            if ATopic.IsDocumented then
            begin
              // ok, create a new dtx topic; corresponding to APasItem; and
              //  combine it with ATopic
              NewTopic := TDtxTopic.Create(FDtxList);
              NewTopic.Name := APasItem.ReferenceName;
              FLinkedList.LeftCount := FLinkedList.LeftCount + 1;
              if not FLinkedList.Link(FDtxList.AddItem(NewTopic), PasIndex) then
                ErrorMsg('Link failed');

              HintMsgFmt('Added <%s> as combine for %s',
               [APasItem2.ReferenceName, NewTopic.Name]);
              NewTopic.Combine := APasItem2.ReferenceName;
              Result := NewTopic.ResolveCombines;
              if not Result then
                Exit;

              Break;
            end;
          end;
        end;
    end;
  end;
end;

function TDtxPasCombiner.AddUndocumented: Boolean;
var
  Topic: TDtxTopic;
  I: Integer;
begin
  if NoSyncWithPas then
  begin
    Result := True;
    Exit;
  end;

  Result := True;
  for I := FDtxList.Count - 1 downto 0 do
    if not FLinkedList.IsLeftLinked[i] and
      //FDtxList.Topics[i].IsDocumented and
      FDtxList.Topics[i].HasPasFileEntry and
      FDtxList.Topics[i].HasTocEntry then
    begin
      HintMsgFmt('Removed <%s> from dtx', [FDtxList.Topics[i].Name]);
      FLinkedList.DeleteLeft(i);
      FDtxList.DeleteItem(i);
    end;

  for I := 0 to FPasParser.TypeList.Count - 1 do
    if not (FPasParser.TypeList[i].DelphiType in cOptionalDelphiTypes) and
      not FPasParser.TypeList[i].IsOptionalInDtx and
      not FLinkedList.IsRightLinked[i] and
      not FDtxList.IsOnSkipList(FPasParser.TypeList[i].ReferenceName) then
    begin
      HintMsgFmt('Added <%s>', [FPasParser.TypeList[i].ReferenceName]);

      Topic := TDtxTopic.Create(FDtxList);
      Topic.Name := FPasParser.TypeList[i].ReferenceName;
      FLinkedList.LeftCount := FLinkedList.LeftCount + 1;
      if not FLinkedList.Link(FDtxList.AddItem(Topic), i) then
        ErrorMsg('Link failed');
    end;
end;

function TDtxPasCombiner.CheckCombinedParams(APasParams: TStrings;
  ADtxTopic: TDtxTopic; const DoNotAdd: Boolean): Boolean;
var
  DtxParams: TStringList;
  NotInDtx, NotInPas: TStringList;
  I: INteger;
  ParamIndex: Integer;
  ADtxParameters: TParameterList;
  Error: Boolean;
  Ok: Boolean;
begin
  Error := False;

  ADtxParameters := ADtxTopic.Parameters;
  DtxParams := TStringList.Create;
  try
    DtxParams.Sorted := True;
    DtxParams.Duplicates := dupAccept;
    for I := 0 to ADtxParameters.Count - 1 do
      DtxParams.Add(ADtxParameters.ParamName[i]);

    Ok := not HasDuplicates(DtxParams);
    if not Ok then
    begin
      ErrorMsgFmt('Duplicate parameter names in <%s>', [ADtxTopic.Name]);
      Error := True;
      // continue;
    end;

    NotInDtx := TStringList.Create;
    NotInPas := TStringList.Create;
    try
      DiffLists(DtxParams, APasParams, nil, NotInDtx, NotInPas);
      for I := 0 to NotInPas.Count - 1 do
      begin
        ParamIndex := ADtxParameters.IndexOf(NotInPas[i]);
        Ok := ParamIndex >= 0;
        if not Ok then
        begin
          // internal error
          ErrorMsgFmt('Could not find parameter <%s>', [NotInPas[i]]);
      Error := True;
      // continue;
        end;

        if ADtxParameters.Params[ParamIndex].IsEmpty then
        begin
          WarningMsgFmt('Removed parameter <%s> from <%s>', [
            ADtxParameters.ParamName[ParamIndex], ADtxTopic.Name]);
          ADtxParameters.Delete(ParamIndex);
        end
        else
        begin
          ErrorMsgFmt('DTX: Parameter <%s> from <%s> not in pas file', [
            ADtxParameters.ParamName[ParamIndex], ADtxTopic.Name]);
      Error := True;
      // continue;
        end
      end;

      // not found
      for I := 0 to NotInDtx.Count - 1 do
      begin
        if DoNotAdd then
          HintMsgFmt('Skipped adding parameter <%s> to <%s> (1)', [
            NotInDtx[I], ADtxTopic.Name])
        else
        begin
          HintMsgFmt('Added parameter <%s> to <%s>', [
            NotInDtx[I], ADtxTopic.Name]);
          ADtxParameters.Add(NotInDtx[I], TSymbolList.Create);
        end;
      end;

      Result := not Error;
    finally
      NotInPas.Free;
      NotInDtx.Free;
    end;
  finally
    DtxParams.Free;
  end;
end;

function TDtxPasCombiner.CheckDuplicatesInDtx: Boolean;
var
  RemoveList: TIntegerList;
  DtxNames: TStringList;
  I, Index: Integer;
  Topic: TDtxTopic;

  OldIndex: Integer;
begin
  Result := True;

  RemoveList := TIntegerList.Create;
  try
    DtxNames := TStringList.Create;
    try
      DtxNames.Sorted := True;
      DtxNames.Duplicates := dupIgnore;

      for I := 0 to FDtxList.Count - 1 do
      begin
        Topic := FDtxList.Topics[i];
        if DtxNames.Find(Topic.Name, Index) then
        begin
          OldIndex := Integer(DtxNames.Objects[Index]);

          if Topic.IsDocumented then
          begin
            if FDtxList.Topics[OldIndex].IsDocumented then
            begin
              // error
              Result := False;
              ErrorMsgFmt('Documented topic <%s> duplicate', [Topic.Name]);
            end
            else
            begin
              // set topic
              // remove old
              DtxNames.Objects[Index] := TObject(I);
              RemoveList.Add(OldIndex);
              WarningMsgFmt('Topic <%s> duplicate', [Topic.Name]);
            end;
          end
          else
          begin
            RemoveList.Add(i);
            WarningMsgFmt('Topic <%s> duplicate', [Topic.Name]);
          end;
        end
        else
          DtxNames.AddObject(Topic.Name, TObject(I));
      end;

      RemoveList.Sort;
      for I := RemoveList.Count - 1 downto 0 do
        FDtxList.DeleteItem(RemoveList[i]);
    finally
      DtxNames.Free;
    end;
  finally
    RemoveList.Free;
  end;
end;

function TDtxPasCombiner.CheckJVCLInfos: Boolean;
var
  I: Integer;
  Topic: TDtxTopic;
  PasIsRegisteredComponent: Boolean;
  DtxIsRegisteredComponent: Boolean;
  GroupStr: string;
  HasSameTitleImgAsName: Boolean;
  HasEmptyTitleImg: Boolean;
begin
  Result := True;

  for I := 0 to FDtxList.Count - 1 do
  begin
    Topic := FDtxList.Topics[i];
    PasIsRegisteredComponent := (Topic.DelphiType = dtClass) and
      FRegisteredComponents.IsRegisteredComponent(Topic.Name);
    DtxIsRegisteredComponent := cifComponent in Topic.Flags;
    //    if not DtxIsRegisteredComponent and (Topic.Flags <> []) then
    //      HintMsgFmt('Flag=<%s> for %s', [Topic.Flag, Topic.Name]);

    HasSameTitleImgAsName := SameText(Topic.TitleImg, Topic.Name);
    HasEmptyTitleImg := IsNullStr(Topic.TitleImg);

    if PasIsRegisteredComponent then
    begin
      if HasEmptyTitleImg then
        HintMsgFmt('Filled TITLEIMG for %s', [Topic.Name])
      else
      if not HasSameTitleImgAsName then
      begin
        //        Result := False;
        //        ErrorMsgFmt('Reg. component %s has invalid TITLEIMG <%s>', [Topic.Name, Topic.TitleImg]);
        //        Exit;
        WarningMsgFmt('Reg. component %s might have invalid TITLEIMG <%s>', [Topic.Name, Topic.TitleImg]);
      end;
    end
    else
    begin
      if HasSameTitleImgAsName then
        HintMsgFmt('Emptied TITLEIMG for %s', [Topic.Name])
      else
        if not HasEmptyTitleImg then
      begin
        Result := False;
        ErrorMsgFmt('Not reg. component %s has TITLEIMG <%s>', [Topic.Name, Topic.TitleImg]);
        Exit;
      end;
    end;

    if PasIsRegisteredComponent then
    begin
      if HasSameTitleImgAsName or HasEmptyTitleImg then
        { fix case }
        Topic.TitleImg := Topic.Name;
    end
    else
      Topic.TitleImg := '';

    if PasIsRegisteredComponent and DtxIsRegisteredComponent then
    begin
      GroupStr := FRegisteredComponents.GroupStr(Topic.Name);
      if not SameText(GroupStr, cUnknownGroupStr) then
      begin
        if not SameText(Topic.Group, GroupStr) then
          WarningMsgFmt('Group changed from <%s> to <%s> for <%s>', [Topic.Group, GroupStr, Topic.Name]);
        Topic.Flags := [cifComponent];
        Topic.Group := GroupStr;
      end;
    end;

    if PasIsRegisteredComponent and not DtxIsRegisteredComponent then
    begin
      Result := not Topic.HasJVCLInfo;
      if Result then
      begin
        Topic.Flags := [cifComponent];
        Topic.Group := FRegisteredComponents.GroupStr(Topic.Name);
        HintMsgFmt('Added JVCLInfo for %s', [Topic.Name]);
      end
      else
      begin
        ErrorMsgFmt('Registered component %s has invalid FLAG in dtx', [Topic.Name]);
        Exit;
      end;
    end;

    if not PasIsRegisteredComponent and DtxIsRegisteredComponent then
    begin
      Result := cifComponent in Topic.Flags;
      if Result then
      begin
        Topic.Flags := [];
        Topic.Group := '';
        WarningMsgFmt('Removed JVCLInfo for %s', [Topic.Name]);
      end
      else
      begin
        ErrorMsgFmt('Unregistered component %s has invalid FLAG in dtx', [Topic.Name]);
        Exit;
      end;
    end;
  end;
end;

function TDtxPasCombiner.CheckMissing: Boolean;
var
  I: Integer;
begin
  if NoSyncWithPas then
  begin
    Result := True;
    Exit;
  end;

  Result := True;

  for I := 0 to FDtxList.Count - 1 do
    if not FLinkedList.IsLeftLinked[i] and
      FDtxList.Topics[i].IsDocumented and
      FDtxList.Topics[i].HasTocEntry and
      FDtxList.Topics[i].HasPasFileEntry then
    begin
      Result := False;
      ErrorMsgFmt('<%s> not in pas file', [FDtxList.Topics[i].Name]);
    end;

  for I := 0 to FPasParser.TypeList.Count - 1 do
    if not (FPasParser.TypeList[i].DelphiType in cOptionalDelphiTypes) and
      not FPasParser.TypeList[i].IsOptionalInDtx and
      not FLinkedList.IsRightLinked[i] and
      not FDtxList.IsOnSkipList(FPasParser.TypeList[i].ReferenceName) then
      HintMsgFmt('<%s> not in dtx file', [FPasParser.TypeList[i].ReferenceName]);
end;

function TDtxPasCombiner.CheckParams(APasParams: TStrings;
  ADtxTopic: TDtxTopic; const DoNotAdd: Boolean): Boolean;
var
  PasIndex: Integer;
  SkipCount: Integer;
  DtxIndex: Integer;
  OkCount: Integer;
  TargetCount: INteger;
  ADtxParameters: TParameterList;
  Error, Ok: Boolean;
begin
  OkCount := 0;
  SkipCount := 0;
  ADtxParameters := ADtxTopic.Parameters;
  Error := False;

  for PasIndex := 0 to APasParams.Count - 1 do
  begin
    DtxIndex := OkCount;
    while (DtxIndex < ADtxParameters.Count) and
      not SameText(ADtxParameters.ParamName[DtxIndex], APasParams[PasIndex]) do
      Inc(DtxIndex);
    if DtxIndex < ADtxParameters.Count then
    begin
      // found
      if PasIndex <> DtxIndex then
        // move item to ok position
        ADtxParameters.MoveParam(DtxIndex, PasIndex);

      Inc(OkCount);
    end
    else
    begin
      // not found
      if DoNotAdd then
      begin
        HintMsgFmt('Skipped adding parameter <%s> to <%s> (2)', [
          APasParams[PasIndex], ADtxTopic.Name]);
        Inc(SkipCount);
      end
      else
      begin
        HintMsgFmt('Added parameter <%s> to <%s>', [
          APasParams[PasIndex], ADtxTopic.Name]);
        ADtxParameters.MoveParam(ADtxParameters.Add(
          APasParams[PasIndex], TSymbolList.Create), PasIndex);
      end;
    end;
  end;

  TargetCount := APasParams.Count;
  while ADtxParameters.Count > TargetCount do
  begin
    if ADtxParameters.Params[TargetCount].IsEmpty then
    begin
      WarningMsgFmt('Removed parameter <%s> from <%s>', [
        ADtxParameters.ParamName[TargetCount], ADtxTopic.Name]);
      ADtxParameters.Delete(TargetCount);
    end
    else
    begin
      ErrorMsgFmt('DTX: Parameter <%s> from <%s> not in pas file', [
        ADtxParameters.ParamName[TargetCount], ADtxTopic.Name]);
      Error := True;
      Inc(TargetCount);
      // continue;
    end
  end;

  Ok := ADtxParameters.Count + SkipCount = TargetCount;
  if not Ok then
  begin
    ErrorMsgFmt('Param count diffs in %s (%d+%d<>%d)',
      [ADtxTopic.Name, ADtxParameters.Count, SkipCount, TargetCount]);
      Error := True;
      // continue;
  end;

  Result := not Error;
end;

function TDtxPasCombiner.CombineFunction(APasItem: TBaseFuncItem;
  ADtxTopic: TDtxTopic): Boolean;
begin
  ADtxTopic.Directives := APasItem.Directives;
  Result := CombineParams(APasItem.Params, ADtxTopic);
end;

function TDtxPasCombiner.CombineItems(APasItem: TAbstractItem;
  ADtxTopic: TDtxTopic): Boolean;
begin
  Result := True;

  ADtxTopic.DelphiType := APasItem.DelphiType;
  if CompareStr(ADtxTopic.Name, APasItem.ReferenceName) <> 0 then
  begin
    HintMsgFmt('<%s> change to <%s>', [ADtxTopic.Name, APasItem.ReferenceName]);
    ADtxTopic.Name := APasItem.ReferenceName;
  end;

  if APasItem is TBaseFuncItem then
    Result := CombineFunction(TBaseFuncItem(APasItem), ADtxTopic)
  else
    if APasItem is TClassItem then
  begin
  end
  else
    if APasItem is TClassMemberOrFieldItem then
  begin
    if APasItem is TParamClassMethodItem then
      Result := CombineMethod(TParamClassMethodItem(APasItem), ADtxTopic)
  end
  else
    if APasItem is TListItem then
    Result := CombineList(TListItem(APasItem), ADtxTopic)
  else
    if APasItem is TValueItem then
  begin
  end;
end;

function TDtxPasCombiner.CombineList(APasItem: TListItem;
  ADtxTopic: TDtxTopic): Boolean;
var
  PasIndex: Integer;
  DtxIndex: Integer;
  OkCount: Integer;
  Topic: TDtxTopic;
  TargetCount: INteger;
begin
  OkCount := 0;
  for PasIndex := 0 to APasItem.Items.Count - 1 do
  begin
    DtxIndex := OkCount;
    while (DtxIndex < ADtxTopic.SubTopicsCount) and
      not SameText(ADtxTopic.SubTopics[DtxIndex].Name, APasItem.Items[PasIndex]) do
      Inc(DtxIndex);
    if DtxIndex < ADtxTopic.SubTopicsCount then
    begin
      // found
      if PasIndex <> DtxIndex then
        // move item to ok position
        ADtxTopic.MoveSubTopic(DtxIndex, PasIndex);

      Inc(OkCount);
    end
    else
    begin
      // not found
      Topic := TDtxTopic.Create(FDtxList);
      Topic.Name := APasItem.Items[PasIndex];
      HintMsgFmt('Added subtopic <%s> to <%s>', [
        APasItem.Items[PasIndex], ADtxTopic.Name]);
      ADtxTopic.MoveSubTopic(ADtxTopic.AddSubTopic(Topic), PasIndex);
    end;
  end;

  TargetCount := APasItem.Items.Count;
  while ADtxTopic.SubTopicsCount > TargetCount do
  begin
    if not ADtxTopic.SubTopics[TargetCount].IsDocumented then
    begin
      WarningMsgFmt('Removed Subtopic <%s> from <%s>', [
        ADtxTopic.SubTopics[TargetCount].Name, ADtxTopic.Name]);
      ADtxTopic.DeleteSubTopic(TargetCount);
    end
    else
    begin
      ErrorMsgFmt('DTX: Subtopic <%s> from <%s> not in pas file', [
        ADtxTopic.SubTopics[TargetCount].Name, ADtxTopic.Name]);
      Result := False;
      Exit;
    end
  end;

  Result := ADtxTopic.SubTopicsCount = TargetCount;
end;

function TDtxPasCombiner.CombineMethod(APasItem: TParamClassMethodItem;
  ADtxTopic: TDtxTopic): Boolean;
begin
  ADtxTopic.Directives := APasItem.Directives;
  if APasItem is TMethodProcItem then
    ADtxTopic.MethodType := TMethodProcItem(APasItem).MethodType;
  Result := CombineParams(APasItem.Params, ADtxTopic);
end;

function TDtxPasCombiner.CombineParams(APasParams: TStrings;
  ADtxTopic: TDtxTopic): Boolean;
var
  AllParams: TStringList;
  I: Integer;
  CombineTopic: TDtxTopic;
  CombinePasItem: TAbstractItem;
  DtxIndex: Integer;
  PasIndex: Integer;
begin
  if NoSyncWithPas then
  begin
    Result := True;
    Exit;
  end;

  if ADtxTopic.CombineTopic <> nil then
  begin
    Result := ADtxTopic.Parameters.IsEmpty;
    if not Result then
    begin
      ErrorMsgFmt('Topic <%s> has COMBINE, but also documented parameters', [ADtxTopic.Name]);
      Exit;
    end;
    { Later }

    CombineTopic := ADtxTopic.CombineTopic;
    if (CombineTopic.CombineWithCount = 1) and (CombineTopic.DelphiType = dtProperty) then
    begin
      Result := CheckParams(APasParams, CombineTopic,
        CombineTopic.IsDocumented and (CombineTopic.Parameters.Count = 0));
      if not Result then
        Exit;
    end;
  end
  else
    if ADtxTopic.CombineWithCount > 0 then
  begin
    AllParams := TStringList.Create;
    try
      AllParams.Duplicates := dupIgnore;
      AllParams.Sorted := True;
      AllParams.CaseSensitive := False;

      AllParams.AddStrings(APasParams);
      for I := 0 to ADtxTopic.CombineWithCount - 1 do
      begin
        CombineTopic := ADtxTopic.CombineWiths[i];
        { TODO : Dirty }
        Result := FDtxList.Find(CombineTopic.Name, DtxIndex);
        if not Result then
        begin
          ErrorMsgFmt('Could not find topic <%s>', [CombineTopic.Name]);
          Exit;
        end;
        PasIndex := FLinkedList.LeftLinkedWith[DtxIndex];
        if PasIndex >= 0 then
        begin
          CombinePasItem := FPasParser.TypeList.Items[PasIndex];

          if CombinePasItem is TBaseFuncItem then
            AllParams.AddStrings(TBaseFuncItem(CombinePasItem).Params)
          else
            if CombinePasItem is TParamClassMethodItem then
            AllParams.AddStrings(TParamClassMethodItem(CombinePasItem).Params)
          else
          begin
            Result := False;
            ErrorMsgFmt('Unexpected type <%s> in TDtxPasCombiner.CombineParams', [CombinePasItem.ClassName]);
            Exit;
          end;
        end;
      end;
      Result := CheckCombinedParams(AllParams, ADtxTopic,
        ADtxTopic.IsDocumented and (ADtxTopic.Parameters.Count = 0));
    finally
      AllParams.Free;
    end;
  end
  else
  begin
    Result := CheckParams(APasParams, ADtxTopic,
      ADtxTopic.IsDocumented and (ADtxTopic.Parameters.Count = 0));
  end;
end;

procedure TDtxPasCombiner.DeterminePercComplete;
var
  I: Integer;
  DocumentedCount: Integer;
begin
  if FDtxList.Count = 0 then
    FPercComplete := 100
  else
  begin
    DocumentedCount := 0;
    for I := 0 to FDtxList.Count - 1 do
      if FDtxList[i].IsDocumented then
        Inc(DocumentedCount);
    FPercComplete := Round(DocumentedCount * 100.0 / FDtxList.Count);
  end;
end;

function TDtxPasCombiner.DoSyncWithOriginal: Boolean;
const
  cWrite = 1;
  cCancel = 2;
  cSkip = 3;
  cButtons: array[0..2] of string = ('Retry', 'Skip', 'Cancel');
  cResults: array[0..2] of Integer = (cWrite, cSkip, cCancel);
var
  OriginalDtxList: TDtxList;
  GenTopic: TDtxTopic;
  OriginalTopic: TDtxTopic;
  I, Index: Integer;
  OriginalChanged: Boolean;
  Action: Integer;
  Combiner: TDtxPasCombiner;
begin
  // add ignore list
  Result := True;
  OriginalChanged := False;

  OriginalDtxList := TDtxList.Create;
  try
    OriginalDtxList.LoadFromFile(OriginalDtxFileName);
    FDtxList.SkipList := OriginalDtxList.SkipList;
    if IsNullStr(FDtxList.PasFileName) then
      FDtxList.PasFileName := OriginalDtxList.PasFileName;
    if IsNullStr(FDtxList.Author) then
      FDtxList.Author := OriginalDtxList.Author;
    if FDtxList.Summary.IsEmpty then
      FDtxList.Summary := OriginalDtxList.Summary;
    if IsNullStr(FDtxList.SeeAlso) then
      FDtxList.SeeAlso := OriginalDtxList.SeeAlso;
    if IsNullStr(FDtxList.Status) then
      FDtxList.Status := OriginalDtxList.Status;

    if UpdateOriginalEmpties then
    begin
      for I := 0 to OriginalDtxList.Count - 1 do
      begin
        OriginalTopic := OriginalDtxList.Topics[i];
        if FDtxList.Find(OriginalTopic.Name, Index) then
        begin
          GenTopic := FDtxList.Topics[Index];
          if OriginalTopic.Summary.IsEmpty and not GenTopic.Summary.IsEmpty then
          begin
            OriginalChanged := True;
            OriginalTopic.Summary.Assign(GenTopic.Summary);
            HintMsgFmt('Updated topic %s', [OriginalTopic.Name]);
          end;
          if OriginalTopic.Description.IsEmpty and not GenTopic.Description.IsEmpty then
          begin
            OriginalChanged := True;
            OriginalTopic.Description.Assign(GenTopic.Description);
            HintMsgFmt('Updated topic %s', [OriginalTopic.Name]);
          end;
        end;
      end;
    end;

    if CopySeeAlsoFromOriginal then
    begin
      for I := 0 to FDtxList.Count - 1 do
      begin
        GenTopic := FDtxList.Topics[i];
        if OriginalDtxList.Find(GenTopic.Name, Index) then
        begin
          OriginalTopic := OriginalDtxList.Topics[Index];
          GenTopic.SeeAlso.Assign(OriginalTopic.SeeAlso);
        end;
      end;
    end;

    if OriginalChanged then
    begin
      Combiner := TDtxPasCombiner.Create;
      try
        Combiner.OnlyCheck := True; // Boolean
        Combiner.GenerateForBuild := False; // Boolean
        Combiner.SourcePasFileName := Self.SourcePasFileName; // string
        Combiner.OriginalDtxFileName := ''; // string
        Combiner.DestDtxFileName := ''; // string
        Combiner.RegisteredComponents := Self.RegisteredComponents; // TRegisteredComponents
        Combiner.PackageList := Self.PackageList; // TStrings
        Combiner.NoSyncWithPas := True; // Boolean
        Combiner.CopyHeaderFromOriginal := False; // Boolean
        Combiner.UpdateOriginalEmpties := False; // Boolean
        Combiner.CopySeeAlsoFromOriginal := False; // Boolean
        Combiner.DtxList := OriginalDtxList; // TDtxList

        Result := Combiner.Execute;
        if not Result then
          Exit;
      finally
        Combiner.Free;
      end;

      Action := cWrite;
      while IsReadOnlyFile(OriginalDtxFileName) do
      begin
        Action := MessageDlgEx(Format('%s is readonly', [OriginalDtxFileName]),
          mtConfirmation, cButtons, cResults, 0);
        case Action of
          cWrite: Continue;
          cCancel:
            begin
              Result := False;
              Exit;
            end;
          cSkip: Break;
        end;
      end;

      if Action = cWrite then
        OriginalDtxList.SaveToDtxFile(OriginalDtxFileName, False);
    end;
  finally
    OriginalDtxList.Free;
  end;
end;

function TDtxPasCombiner.Execute: Boolean;
begin
  Result := Parse;
  if not Result then
    Exit;

  Result := Process;
  if not Result then
    Exit;

  if not OnlyCheck then
  begin
    //    Result := SaveXML;
    //    if not Result then
    //      Exit;

    Result := SaveDtx;
    if not Result then
      Exit;
  end;
end;

function TDtxPasCombiner.ExpandMissingDtxNames: Boolean;
var
  DtxIndex: Integer;
  ATopic: TDtxTopic;
  APasItem: TAbstractItem;
  PasIndex: Integer;
begin
  if NoSyncWithPas then
  begin
    Result := True;
    Exit;
  end;

  for DtxIndex := 0 to FDtxList.Count - 1 do
  begin
    ATopic := FDtxList.Topics[DtxIndex];

    if not FLinkedList.IsLeftLinked[DtxIndex] and
      {FDtxList.Topics[DtxIndex].IsDocumented and}
      ATopic.HasTocEntry and
      ATopic.HasPasFileEntry then
    begin
      // can it be expanded?
      PasIndex := FPasParser.TypeList.IndexOfLinkName(ATopic.Name);
      if PasIndex >= 0 then
      begin
        APasItem := FPasParser.TypeList[PasIndex];

        if not FLinkedList.IsRightLinked[PasIndex] then
        begin
          HintMsgFmt('<%s> changed to <%s>', [ATopic.Name, APasItem.ReferenceName]);
          ATopic.Name := APasItem.ReferenceName;

          // link it
          Result := FLinkedList.Link(DtxIndex, PasIndex);
          if not Result then
              ErrorMsg('Link failed');
        end;
      end;
    end;
  end;

  Result := True;
end;

function TDtxPasCombiner.ExpandPasDuplicates: Boolean;
var
  I, J: Integer;
  PasItem1, PasItem2: TAbstractItem;
begin
  // some items can have the same name without the overload directive, due
  // to $IFDEF's

  for I := 0 to FPasParser.TypeList.Count - 1 do
  begin
    PasItem1 := FPasParser.TypeList[i];
    for J  := 0 to I-1 do
    begin
      PasItem2 := FPasParser.TypeList[j];
      if SameText(PasItem1.ReferenceName,PasItem2.ReferenceName) then
      begin
        PasItem1.ExpandName := True;
        PasItem2.ExpandName := True;
      end;
    end;
  end;

  Result := True;
end;

function TDtxPasCombiner.FillInDefaultsForBuild: Boolean;
var
  I, J: Integer;
  Topic: TDtxTopic;
  TopicIsDocumented: Boolean;
  ComponentName: string;
begin
  Result := True;

  if IsNullStr(FDtxList.Author) then
    FDtxList.Author := Trim(FPasParser.TypeList.Author);
  if FDtxList.Summary.IsEmpty then
  begin
    ComponentName := 'T' + ChangeFileExt(FDtxList.PasFileName, '');
    for I := 0 to FDtxList.Count - 1 do
      if (FDtxList[i].DelphiType = dtClass) and SameText(ComponentName, FDtxList[i].Name) then
      begin
        ComponentName := FDtxList[i].Name;
        Break;
      end;
    FDtxList.Summary.Parse(Format('Contains the %s component.', [ComponentName]));
  end;

  for I := 0 to FDtxList.Count - 1 do
  begin
    Topic := FDtxList.Topics[i];
    if (Topic.CombineTopic = nil) and
      IsNullStr(Topic.Alias) and
      (Topic.DelphiType <> dtOther) then
    begin
      TopicIsDocumented := Topic.IsDocumented;
      if not TopicIsDocumented then
      begin
        //        if Topic.DelphiType <> dtClass then
        //          Topic.SeeAlso.Parse(cSeeAlsoDefaultText);
        //        if Topic.DelphiType = dtMethodFunc then
        //          Topic.ReturnValue.Parse(cReturnValueDefaultText);
      end;

      if Topic.Summary.IsEmpty then
        Topic.Summary.Parse(cSummaryDefaultTextForBuild);
      if Topic.Description.IsEmpty then
      begin
        //        if diOverride in Topic.Directives then
        //          Topic.Description.Parse(cDescriptionOverrideDefaultText)
        //        else
        //        if diOverload in Topic.Directives then
        //          Topic.Description.Parse(cDescriptionOverloadDefaultText)
        //        else

        // note: EXTLINK may not have spaces.
        Topic.Description.Parse(Format(cDescriptionDefaultTextForBuild, [StripOverloadAt(Topic.Name)]))
      end;

      for j := 0 to Topic.Parameters.Count - 1 do
      begin
        if Topic.Parameters.Params[j].IsEmpty then
          Topic.Parameters.Params[j].Parse(cParameterDefaultTextForBuild);
      end;
      for j := 0 to Topic.SubTopicsCount - 1 do
      begin
        if Topic.SubTopics[j].Summary.IsEmpty then
          Topic.SubTopics[j].Summary.Parse(cEnumerateDefaultTextForBuild);
      end;
    end;
  end;
end;

function TDtxPasCombiner.FillInDefaultsForGen: Boolean;
var
  I, J: Integer;
  Topic: TDtxTopic;
  TopicIsDocumented: Boolean;
  ComponentName: string;
begin
  Result := True;

  if IsNullStr(FDtxList.Author) then
    FDtxList.Author := Trim(FPasParser.TypeList.Author);
  if FDtxList.Summary.IsEmpty then
  begin
    ComponentName := 'T' + ChangeFileExt(FDtxList.PasFileName, '');
    for I := 0 to FDtxList.Count - 1 do
      if (FDtxList[i].DelphiType = dtClass) and SameText(ComponentName, FDtxList[i].Name) then
      begin
        ComponentName := FDtxList[i].Name;
        Break;
      end;
    FDtxList.Summary.Parse(Format('Contains the %s component.', [ComponentName]));
  end;
  for I := 0 to FDtxList.Count - 1 do
  begin
    Topic := FDtxList.Topics[i];
    if (Topic.CombineTopic = nil) and
      IsNullStr(Topic.Alias) and
      (Topic.DelphiType <> dtOther) then
    begin
      TopicIsDocumented := Topic.IsDocumented;
      if not TopicIsDocumented then
      begin
        if Topic.DelphiType <> dtClass then
//          Topic.SeeAlso.Parse(cSeeAlsoDefaultText);
           {special }
           Topic.SeeAlso.Add(TSpecialStringSymbol.CreateNew(
    'List here other properties, methods (comma separated)'#13#10'Remove the ''See Also'' section if there are no references'));
        if Topic.DelphiType = dtMethodFunc then
          Topic.ReturnValue.Parse(cReturnValueDefaultText);
      end;

      if Topic.Summary.IsEmpty then
        Topic.Summary.Parse(cSummaryDefaultText);
      if Topic.Description.IsEmpty then
      begin
        if diOverride in Topic.Directives then
          Topic.Description.Parse(cDescriptionOverrideDefaultText)
        else
          if diOverload in Topic.Directives then
          Topic.Description.Parse(cDescriptionOverloadDefaultText)
        else
          Topic.Description.Parse(cDescriptionDefaultText)
      end;

      for j := 0 to Topic.Parameters.Count - 1 do
      begin
        if Topic.Parameters.Params[j].IsEmpty then
          Topic.Parameters.Params[j].Parse(cParameterDefaultText);
      end;
      for j := 0 to Topic.SubTopicsCount - 1 do
      begin
        if Topic.SubTopics[j].Summary.IsEmpty then
          Topic.SubTopics[j].Summary.Parse(Format('Description for %s', [Topic.SubTopics[j].Name]));
      end;
    end;
  end;
end;

function TDtxPasCombiner.FillInDefaultTitles: Boolean;
var
  I: Integer;
  Topic: TDtxTopic;
  ATopicName: string;
begin
  Result := True;

  for I := 0 to FDtxList.Count - 1 do
  begin
    Topic := FDtxList.Topics[i];
    if IsNullStr(Topic.Title) then
      case Topic.DelphiType of
        dtVar:
          Topic.Title := Format('%s variable', [Topic.Name]);
        dtEnum, dtFunctionType, dtRecord, dtProcedureType, dtType, dtMetaClass:
          Topic.Title := Format('%s type', [Topic.Name]);
        dtProcedure:
          begin
            ATopicName := StripOverloadAt(Topic.Name);
            if IsNullStr(Topic.Combine) or not SameText(ATopicName, StripOverloadAt(Topic.Combine)) then
              Topic.Title := Format('%s procedure', [ATopicName]);
          end;
        dtFunction:
          begin
            ATopicName := StripOverloadAt(Topic.Name);
            if IsNullStr(Topic.Combine) or not SameText(ATopicName, StripOverloadAt(Topic.Combine)) then
              Topic.Title := Format('%s function', [ATopicName]);
          end;
      end
    else
      if HasAt(Topic.Title) then
    begin
      ErrorMsgFmt('Topic <%s> has @ in title: <%s>', [Topic.Name, Topic.Title]);
      Result := False;
      Exit;
    end;
  end;
end;

function TDtxPasCombiner.FillLinkedList: Boolean;
var
  I: Integer;
  PasItem: TAbstractItem;
  DtxItem: TDtxTopic;
  Index: Integer;
begin
  Result := True;

  // left = dtx
  // right = pas
  FLinkedList.LeftCount := FDtxList.Count;
  FLinkedList.RightCount := FPasParser.TypeList.Count;
  FLinkedList.Reset;

  for I := 0 to FPasParser.TypeList.Count - 1 do
  begin
    PasItem := FPasParser.TypeList[i];
    if FDtxList.Find(PasItem.ReferenceName, Index) then
    begin
      Result := FLinkedList.Link(Index, I);
      if not Result then
      begin
        ErrorMsgFmt('PAS: Could not link <%s>', [PasItem.ReferenceName]);
        Exit;
      end;
    end;
  end;

  for I := 0 to FDtxList.Count - 1 do
    if not FLinkedList.IsLeftLinked[I] then
    begin
      DtxItem := FDtxList.Topics[i];
      Index := FPasParser.TypeList.IndexOfReferenceName(DtxItem.Name);
      if Index >= 0 then
      begin
        FLinkedList.Link(I, Index);
        begin
          ErrorMsgFmt('DTX: Could not link <%s>', [DtxItem.Name]);
          Exit;
        end;
      end;
    end;
end;

function TDtxPasCombiner.GroupListItem(APasItem: TListItem): Boolean;
var
  Index: Integer;
  ReferenceName: string;
  I: Integer;
  ADtxTopic: TDtxTopic;
  SubTopic: TDtxTopic;
begin
  Result := True;

  if not FDtxList.Find(APasItem.ReferenceName, Index) then
    Exit;

  ADtxTopic := FDtxList.Topics[Index];

  for I := 0 to APasItem.Items.Count - 1 do
  begin
    ReferenceName := APasItem.ReferenceName + '.' + APasItem.Items[i];
    if FDtxList.Find(ReferenceName, Index) then
    begin
      SubTopic := FDtxList.Topics[Index];
      SubTopic.Name := APasItem.Items[i];
      if not SubTopic.SeeAlso.IsEmpty then
      begin
        HintMsgFmt('See also of <%s> moved to <%s>', [SubTopic.Name, ADtxTopic.Name]);
        SubTopic.SeeAlso.MoveTo(ADtxTopic.SeeAlso);
      end;
      FDtxList.MoveItemToSubItem(Index, ADtxTopic);
    end;
  end;
end;

function TDtxPasCombiner.GroupListItems: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FPasParser.TypeList.Count - 1 do
    if FPasParser.TypeList[i] is TListItem then
    begin
      Result := GroupListItem(TListItem(FPasParser.TypeList[i]));
      if not Result then
        Exit;
    end;
end;

function TDtxPasCombiner.Parse: Boolean;
begin
  Result := Assigned(FRegisteredComponents);
  if not Result then
    Exit;

  FPasParser.AcceptCompilerDirectives := DefaultAcceptCompilerDirectives;
  FPasParser.AcceptVisibilities := [inProtected, inPublic, inPublished];

  Result := FPasParser.ExecuteFile(SourcePasFileName);
  if not Result then
  begin
    ErrorMsgFmt('Error parsing %s', [SourcePasFileName]);
    Exit;
  end;

  { JCL specific stuff }
  Result := ExpandPasDuplicates;
  if not Result then Exit;

  Result := RemovePasDuplicates;
  if not Result then Exit;

  //  Result := FDtxList.LoadFromFile(SourceDtxFileName);
  //  if not Result then
  //  begin
  //    ErrorMsgFmt('Error parsing %s', [SourceDtxFileName]);
  //    Exit;
  //  end;
end;

function TDtxPasCombiner.Process: Boolean;
begin
  Result := CheckPackage;
  if not Result then
    Exit;

  Result := CheckDuplicatesInDtx;
  if not Result then
    Exit;

  Result := GroupListItems;
  if not Result then
    Exit;

  Result := FillLinkedList;
  if not Result then
    Exit;

  Result := ExpandMissingDtxNames;
  if not Result then
  Exit;

  Result := AddExpandCombines;
  if not Result then Exit;


  Result := SetHASPASFILEENTRYForJCLTopics;
  if not Result then Exit;

  Result := CheckMissing;
  if not Result then
    Exit;


  if CopyHeaderFromOriginal or UpdateOriginalEmpties or CopySeeAlsoFromOriginal then
  begin
    // copies skip list; do for AddUndocumented.
    Result := DoSyncWithOriginal;
    if not Result then
      Exit;
  end;

  Result := AddUndocumented;
  if not Result then
    Exit;

  //  Result := FLinkedList.LeftUnlinkedCount = 0;
  //  if not Result then
  //    Exit;

  DeterminePercComplete;

  Result := FDtxList.ResolveCombines;
  if not Result then
    Exit;

  Result := AddDefaultCombines;
  if not Result then
    Exit;

  Result := ProcessLinks;
  if not Result then
    Exit;

  Result := CheckJVCLInfos;
  if not Result then
    Exit;

  // must be fore FillInDefaultTitles
  if GenerateForBuild then
  begin
    Result := FillInDefaultsForBuild;
    if not Result then
      Exit;

    Result := ProcessSeeAlsosForBuild;
    if not Result then
      Exit;
  end
  else
  begin
    Result := FillInDefaultsForGen;
    if not Result then
      Exit;
  end;

  Result := FillInDefaultTitles;
  if not Result then
    Exit;
end;

function TDtxPasCombiner.ProcessLinks: Boolean;
var
  I: Integer;
  PasIndex: Integer;
  Ok, Error: Boolean;
begin
  Error := False;

  // preprocess delphi types
  for I := 0 to FDtxList.Count - 1 do
  begin
    PasIndex := Flinkedlist.LeftLinkedWith[i];
    if PasIndex < 0 then
    begin
      // -1, if not linked, this is ok for examples etc. see for example JvID3v1.dtx
    end
    else
    begin
      FDtxList.Topics[i].DelphiType := FPasParser.TypeList.Items[PasIndex].DelphiType;
    end;
  end;

  for I := 0 to FDtxList.Count - 1 do
  begin
    PasIndex := Flinkedlist.LeftLinkedWith[i];
    if PasIndex < 0 then
    begin
      // -1, if not linked, this is ok for examples etc. see for example JvID3v1.dtx
    end
    else
    begin
      Ok  := CombineItems(FPasParser.TypeList.Items[PasIndex], FDtxList.Topics[i]);
      if not Ok then
        Error := True;
    end;
  end;

  Result := not Error;
end;

function TDtxPasCombiner.SaveDtx: Boolean;
begin
  Result := True;
  FDtxList.SaveToDtxFile(DestDtxFileName, GenerateForBuild);
end;

function TDtxPasCombiner.SaveXML: Boolean;
begin
  Result := True;
  FDtxList.SaveToXMLFile(DestXMLFileName);
end;

function TDtxPasCombiner.SetHASPASFILEENTRYForJCLTopics: Boolean;
var
  DtxIndex: Integer;
  ATopic: TDtxTopic;
begin
  if NoSyncWithPas then
  begin
    Result := True;
    Exit;
  end;

  Result := True;
  for DtxIndex := 0 to FDtxList.Count - 1 do
  begin
    ATopic := FDtxList.Topics[DtxIndex];
    if not FLinkedList.IsLeftLinked[DtxIndex] and
       ATopic.HasPasFileEntry and
       (ATopic.Group2 <> '') and
       (ATopic.TOPICORDER <> '') and
       (ATopic.Title <> '') and
       ATopic.Description.IsEmpty and ATopic.Summary.IsEmpty then
    begin
      // this seems to be a JCL topic;
      ATopic.HasPasFileEntry := False;
    end;
  end;
end;

//=== { TLinkedList } ========================================================

constructor TLinkedList.Create;
begin
  inherited create;
  FLeftIndexen := TIntegerList.Create;
  FRightIndexen := TIntegerList.Create;
end;

destructor TLinkedList.Destroy;
begin
  FLeftIndexen.Free;
  FRightIndexen.Free;
  inherited Destroy;
end;

procedure TLinkedList.DeleteLeft(const ALeftIndex: Integer);
var
  I: Integer;
begin
  if LeftLinkedWith[ALeftIndex] = -1 then
    Dec(FLeftUnlinkedCount)
  else
    LeftLinkedWith[ALeftIndex] := -1;
  FLeftIndexen.Delete(ALeftIndex);
  for I := 0 to RightCount - 1 do
    if FRightIndexen[i] > ALeftIndex then
      FRightIndexen[i] := FRightIndexen[i] - 1;
end;

procedure TLinkedList.DeleteRight(const ARightIndex: Integer);
var
  I: Integer;
begin
  if RightLinkedWith[ARightIndex] = -1 then
    Dec(FRightUnlinkedCount)
  else
    RightLinkedWith[ARightIndex] := -1;
  FRightIndexen.Delete(ARightIndex);
  for I := 0 to LeftCount - 1 do
    if FLeftIndexen[i] > ARightIndex then
      FLeftIndexen[i] := FLeftIndexen[i] - 1;
end;

function TLinkedList.GetIsLeftLinked(const ALeftIndex: Integer): Boolean;
begin
  Result := FLeftIndexen[ALeftIndex] >= 0;
end;

function TLinkedList.GetIsRightLinked(const ARightIndex: Integer): Boolean;
begin
  Result := FRightIndexen[ARightIndex] >= 0;
end;

function TLinkedList.GetLeftCount: Integer;
begin
  Result := FLeftIndexen.Count;
end;

function TLinkedList.GetLeftLinkedWith(const ALeftIndex: Integer): Integer;
begin
  Result := FLeftIndexen[ALeftIndex];
end;

function TLinkedList.GetRightCount: Integer;
begin
  Result := FRightIndexen.Count;
end;

function TLinkedList.GetRightLinkedWith(
  const ARightIndex: Integer): Integer;
begin
  Result := FRightIndexen[ARightIndex];
end;

function TLinkedList.Link(const ALeftIndex, ARightIndex: Integer): Boolean;
begin
  Result := not IsLeftLinked[ALeftIndex] and not IsRightLinked[ARightIndex];
  if Result then
  begin
    FLeftIndexen[ALeftIndex] := ARightIndex;
    FRightIndexen[ARightIndex] := ALeftIndex;

    Dec(FLeftUnlinkedCount);
    Dec(FRightUnlinkedCount);
  end
  else
    Result :=
      (FLeftIndexen[ALeftIndex] = ARightIndex) and
      (FRightIndexen[ARightIndex] = ALeftIndex);
end;

procedure TLinkedList.Reset;
var
  I: Integer;
begin
  for I := 0 to RightCount - 1 do
    FRightIndexen[i] := -1;
  for I := 0 to LeftCount - 1 do
    FLeftIndexen[i] := -1;
  FLeftUnlinkedCount := LeftCount;
  FRightUnlinkedCount := RightCount;
end;

procedure TLinkedList.SetLeftCount(const Value: Integer);
var
  OldCount: Integer;
  I: Integer;
begin
  OldCount := FLeftIndexen.Count;
  FLeftIndexen.Count := Value;
  for I := OldCount to Value - 1 do
    FLeftIndexen[i] := -1;
end;

procedure TLinkedList.SetLeftLinkedWith(const ALeftIndex, Value: Integer);
var
  CurrentRightIndex: Integer;
begin
  CurrentRightIndex := LeftLinkedWith[ALeftIndex];
  if CurrentRightIndex <> Value then
  begin
    if CurrentRightIndex <> -1 then
      Unlink(ALeftIndex, CurrentRightIndex);

    if not Link(ALeftIndex, Value) then
      ErrorMsg('Link failed');
  end;
end;

procedure TLinkedList.SetRightCount(const Value: Integer);
var
  OldCount: Integer;
  I: Integer;
begin
  OldCount := FRightIndexen.Count;
  FRightIndexen.Count := Value;
  for I := OldCount to Value - 1 do
    FRightIndexen[i] := -1;
end;

procedure TLinkedList.SetRightLinkedWith(const ARightIndex,
  Value: Integer);
var
  CurrentLeftIndex: Integer;
begin
  CurrentLeftIndex := RightLinkedWith[ARightIndex];
  if CurrentLeftIndex <> Value then
  begin
    if CurrentLeftIndex <> -1 then
      Unlink(ARightIndex, CurrentLeftIndex);

    if not Link(ARightIndex, Value) then
      ErrorMsg('Link failed');
  end;
end;

function TLinkedList.UnLink(const ALeftIndex,
  ARightIndex: Integer): Boolean;
begin
  Result := (LeftLinkedWith[ALeftIndex] = ARightIndex) and
    (RightLinkedWith[ARightIndex] = ALEftIndex);
  if Result then
  begin
    FLeftIndexen[ALeftIndex] := -1;
    FRightIndexen[ARightIndex] := -1;

    Inc(FLeftUnlinkedCount);
    Inc(FRightUnlinkedCount);
  end
  else
    Result :=
      (FLeftIndexen[ALeftIndex] = -1) and
      (FRightIndexen[ARightIndex] = -1);
end;

function TDtxPasCombiner.CheckPackage: Boolean;
var
  ExpPackageName: string;
begin
  Result := Assigned(PackageList);
  if not Result then
    Exit;

  if FDtxList.Package = '??' then
    FDtxList.Package := '';
  ExpPackageName := PackageList.Values[ChangeFileExt(FDtxList.PasFileName, '')];

  if not SameText(FDtxList.Package, ExpPackageName) then
  begin
    if IsNullStr(FDtxList.Package) then
    begin
      HintMsgFmt('Filled package (%s) for %s', [ExpPackageName, FDtxList.PasFileName]);
      FDtxList.Package := ExpPackageName
    end
    else
      if IsNullStr(ExpPackageName) then
    begin
      WarningMsgFmt('Package is %s (should be empty) for %s', [FDtxList.Package, FDtxList.PasFileName]);
    end
    else
    begin
      WarningMsgFmt('Package changed from %s to %s for %s', [FDtxList.Package, ExpPackageName, FDtxList.PasFileName]);
      FDtxList.Package := ExpPackageName
    end;
  end;

  if IsNullStr(FDtxList.Package) then
    FDtxList.Package := '??';
end;

procedure EnsureLastIsComma(List: TSymbolList);
begin
  if List.Count >= 1 then
  begin
    if List[List.Count - 1] is TStringSymbol then
    begin
    end
    else
    begin
      List.Add(TStringSymbol.CreateNew(', '));
    end;
  end;
end;

function IsValidDotLessLink(const S: string): Boolean;
begin
  // TJv, EJv, IJv
  Result := (Length(S) > 3) and
    ((S[1] = 'T') or (S[1] = 'E') or (S[1] = 'I')) and
    (S[2] = 'J') and (S[3] = 'v');
end;

procedure TDtxPasCombiner.ParseSeeAlsoText(List: TSymbolList; const SeeAlsoText: string);
const
  cWhiteSpace = [#1..#32] + [','];
var
  P, Q: PChar;
  S: string;
  DotCount: Integer;
begin
  P := PChar(SeeAlsoText);
  while P^ <> #0 do
  begin
    // skip whitespace
    while P^ in cWhiteSpace do
      Inc(P);
    Q := P;
    DotCount := 0;
    while not (P^ in cWhiteSpace + [#0]) do
    begin
      if P^ = '.' then
        Inc(DotCount);
      Inc(P);
    end;
    if P <> Q then
    begin
      EnsureLastIsComma(List);
      SetString(S, Q, P - Q);
      if ((DotCount = 0) and not IsValidDotLessLink(S)) or
        ((DotCount <> 0) and (DotCount <> 1)) then
      begin
        WarningMsgFmt('See Also Link %s has %d dots', [QuotedStr(S), DotCount]);
      end;
      List.Add(TLinkSymbol.CreateNew(S, ''))
    end;
  end;
end;

function TDtxPasCombiner.ProcessSeeAlsosForBuild: Boolean;
var
  I, J: Integer;
  ATopic: TDtxTopic;
  NewList: TSymbolList;
  IsLink, IsDelphiLink: Boolean;
begin
  Result := True;
  for I := 0 to FDtxList.Count - 1 do
  begin
    ATopic := FDtxList.Topics[i];
    if not ATopic.SeeAlso.IsEmpty then
    begin
      NewList := TSymbolList.Create(True);
      try
        for J := 0 to ATopic.SeeAlso.Count - 1 do
          if ATopic.SeeAlso[J] is TStringSymbol then
            ParseSeeAlsoText(NewList, ATopic.SeeAlso[j].DtxText)
          else
          begin
            IsLink := ATopic.SeeAlso[j] is TLinkSymbol;
            IsDelphiLink := ATopic.SeeAlso[j] is TDelphiLinkSymbol;

            if IsLink then
              WarningMsgFmt('Topic %s contains a link in the SEE ALSO section', [QuotedStr(ATopic.Name)]);
            //            if not IsLink and not IsDelphiLink then
            //              WarningMsgFmt('Topic %s contains an unexpected item: (%s) %s', [
            //                QuotedStr(ATopic.Name),
            //                ATopic.SeeAlso[j].ClassName,
            //                QuotedStr(ATopic.SeeAlso[j].DtxText)]);
            if IsLink or IsDelphiLink then
              EnsureLastIsComma(NewList);
            if IsDelphiLink then
              NewList.Add(TStringSymbol.CreateNew(TDelphiLinkSymbol(ATopic.SeeAlso[j]).Link))
            else
              NewList.Add(ATopic.SeeAlso[J].ConstructCopy);
          end;
        ATopic.SeeAlso.Assign(NewList);
      finally
        NewList.Free;
      end;
    end;
  end;
end;

function TDtxPasCombiner.RemovePasDuplicates: Boolean;
var
  I, J: Integer;
  PasItem1, PasItem2: TAbstractItem;
begin
  // some items can have the same name without the overload directive, due
  // to $IFDEF's

  for I := FPasParser.TypeList.Count - 1 downto 0 do
  begin
    PasItem1 := FPasParser.TypeList[i];
    for J  := 0 to I-1 do
    begin
      PasItem2 := FPasParser.TypeList[j];
      if SameText(PasItem1.ReferenceName,PasItem2.ReferenceName) then
      begin
        WarningMsgFmt('Pas file contains duplicate item <%s>', [PasItem1.ReferenceName]);
        FPasParser.TypeList.Delete(I);
        Break;
      end;
    end;
  end;

  Result := True;
end;

initialization
finalization
  FreeAndNil(GDefaultAcceptCompilerDirectives);
end.




