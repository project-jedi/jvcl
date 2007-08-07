unit GroupDtxProducer;

interface

uses
  Classes, ContNrs, JVCLHelpUtils, DtxParser, DtxAnalyzer;

type
  TGroupDtxProducer = class(TTask)
  private
    FGroupInfos: TGroupInfos;
    FComponentInfos: TComponentInfos;
    FGroupsDtxFileName: string;
    FNoLinks: Boolean;
    function IsNotDocumented(ASymbolList: TSymbolList): Boolean;
  protected
    function CountComp(AGroupInfo: TGroupInfo): Integer;
    function CountClass(AGroupInfo: TGroupInfo): Integer;
    function CountRoutines(AGroupInfo: TGroupInfo): Integer;

    procedure SaveGroupInfoDescription(AGroupInfo: TGroupInfo; ADtxWriter: TDtxWriter);
    procedure SaveGroupInfoComponentList(AGroupInfo: TGroupInfo; ADtxWriter: TDtxWriter);
    procedure SaveGroupInfoClassList(AGroupInfo: TGroupInfo; ADtxWriter: TDtxWriter);
    procedure SaveGroupInfoRoutineList(AGroupInfo: TGroupInfo; ADtxWriter: TDtxWriter);
    procedure SaveGroupInfo(AGroupInfo: TGroupInfo; ADtxWriter: TDtxWriter; const TopicOrder: Integer);
    procedure SaveGroupInfos(AGroupInfos: TGroupInfos; ADtxWriter: TDtxWriter);
    procedure SaveDtx(ADtxWriter: TDtxWriter);

    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    property GroupInfos: TGroupInfos read FGroupInfos write FGroupInfos;
    property ComponentInfos: TComponentInfos read FComponentInfos write FComponentInfos;
    property GroupsDtxFileName: string read FGroupsDtxFileName write FGroupsDtxFileName;
    property NoLinks: Boolean read FNoLinks write FNoLinks;
  end;

implementation

uses
  SysUtils, ParserTypes;

//const
//  cifSymbolType = [cifComponent, cifClass, cifRoutine];

function TGroupDtxProducer.CanStart: Boolean;
begin
  Result := not IsNullStr(GroupsDtxFileName) and Assigned(GroupInfos) and
    Assigned(ComponentInfos);
end;

function TGroupDtxProducer.CountClass(AGroupInfo: TGroupInfo): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := AGroupInfo.Components.Count - 1 downto 0 do
    if cifClass in ComponentInfos.ItemByName[AGroupInfo.Components[I]].Flags then
      Inc(Result);
end;

function TGroupDtxProducer.CountComp(AGroupInfo: TGroupInfo): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := AGroupInfo.Components.Count - 1 downto 0 do
    if cifComponent in ComponentInfos.ItemByName[AGroupInfo.Components[I]].Flags then
      Inc(Result);
end;

function TGroupDtxProducer.CountRoutines(AGroupInfo: TGroupInfo): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := AGroupInfo.Components.Count - 1 downto 0 do
    if cifRoutine in ComponentInfos.ItemByName[AGroupInfo.Components[I]].Flags then
      Inc(Result);
end;

function TGroupDtxProducer.DoExecute: Boolean;
var
  Stream: TFileStream;
  DtxWriter: TDtxWriter;
begin
  Result := True;

  Stream := TFileStream.Create(GroupsDtxFileName, fmCreate);
  try
    DtxWriter := TDtxWriter.Create;
    try
      DtxWriter.DestStream := Stream;
      SaveDtx(DtxWriter);
    finally
      DtxWriter.Free;
    end;
  finally
    Stream.Free;
  end;
end;

function TGroupDtxProducer.GetTaskDescription: string;
begin
  Result := 'Genererating function reference';
end;

function TGroupDtxProducer.IsNotDocumented(
  ASymbolList: TSymbolList): Boolean;
begin
  Result := Assigned(ASymbolList) and (ASymbolList.Count = 1) and
    (ASymbolList[0] is TStringSymbol) and
    (SameText(TStringSymbol(ASymbolList[0]).Value, cSummaryDefaultTextForBuild));
end;

procedure TGroupDtxProducer.SaveDtx(ADtxWriter: TDtxWriter);
begin
  ADtxWriter.WriteLn(StringOfChar('#', 100));
  ADtxWriter.WriteComment(Pad(' JEDI-VCL help: Functional reference include file.', 94, ' ') + ' ##');
  ADtxWriter.WriteComment(Pad(' Include file generated on ' +
    FormatDateTime('dd-mm-yyyy "at" hh:nn:ss', Now), 94, ' ') + ' ##');
  ADtxWriter.Writeln(StringOfChar('#', 100));
  ADtxWriter.WriteTopicName('$JVCL.FuncRef');
  ADtxWriter.WriteLn('<GROUP $JVCL>');
  ADtxWriter.WriteTitle('Functional Reference');
  ADtxWriter.BeginSection('Description');
  ADtxWriter.WriteWrappedData(
    'JEDI-VCL consists of many components and controls for all sorts of things. To help you find the ' +
    'right component for a particular task, we have categorized the controls and components. Some ' +
    'components are in multiple categories if there was any overlap in functionality of the ' +
    'component.');
  ADtxWriter.EndSection;
  ADtxWriter.WriteSepLine;

  SaveGroupInfos(FGroupInfos, ADtxWriter);
  //  FileSL.SaveToFile(HelpPath + 'generated includes\JVCL.FuncRef.dtx');
end;

procedure TGroupDtxProducer.SaveGroupInfo(AGroupInfo: TGroupInfo;
  ADtxWriter: TDtxWriter; const TopicOrder: Integer);
begin
  with AGroupInfo do
  begin
    ADtxWriter.WriteTopicName('$' + ParentGroupID + '.' + GroupID);
    ADtxWriter.WriteLnFmt('<GROUP $%s>', [ParentGroupID(Parent = nil)]);
    ADtxWriter.WriteLnFmt('<TOPICORDER %d>', [TopicOrder]);
    ADtxWriter.WriteTitle(AGroupInfo.GroupTitle);
    ADtxWriter.BeginSection('Description');
    SaveGroupInfoDescription(AGroupInfo, ADtxWriter);
    SaveGroupInfoComponentList(AGroupInfo, ADtxWriter);
    SaveGroupInfoClassList(AGroupInfo, ADtxWriter);
    SaveGroupInfoRoutineList(AGroupInfo, ADtxWriter);
    ADtxWriter.EndSection;
    ADtxWriter.WriteLn;
    ADtxWriter.WriteSepLine;

    SaveGroupInfos(SubGroups, ADtxWriter);
  end;
end;

procedure TGroupDtxProducer.SaveGroupInfoClassList(AGroupInfo: TGroupInfo;
  ADtxWriter: TDtxWriter);
var
  I: Integer;
  Comp: TComponentInfo;
  DtxTable: TDtxTable;
  RowCount: Integer;
  SymbolList: TSymbolList;
begin
  if CountClass(AGroupInfo) = 0 then
    Exit;
  if CountComp(AGroupInfo) > 0 then
  begin
    ADtxWriter.WriteLn;
    ADtxWriter.WriteLn;
    ADtxWriter.WriteWrappedData('In addition to these components, the following classes belong to this group:');
  end
  else
    ADtxWriter.WriteWrappedData('This group contains the following classes:');

  RowCount := 0;

  DtxTable := TDtxTable.Create;
  try
    DtxTable.ColCount := 2;
    DtxTable.RowCount := AGroupInfo.Components.Count;
    DtxTable.Header[0] := 'Class';
    DtxTable.Header[1] := 'Description';
    DtxTable.HasHeader := True;

    for I := 0 to AGroupInfo.Components.Count - 1 do
    begin
      Comp := ComponentInfos[ComponentInfos.IndexOf(AGroupInfo.Components[I])];
      if cifClass in Comp.Flags then
      begin
        if IsNotDocumented(Comp.Summary) then
        begin
          DtxTable.Cells[0, RowCount] := Comp.Name;

          SymbolList := TSymbolList.Create;
          SymbolList.Parse(Format('Not documented (' + cEditLink + ').', [Comp.Name, 'Edit topic']));

          DtxTable.CellsSymbols[1, RowCount] := SymbolList;
        end
        else
        begin
          if NoLinks then
            DtxTable.Cells[0, RowCount] else
            DtxTable.Cells[0, RowCount] := LinkStr(Comp.Name);
          DtxTable.CellsSymbols[1, RowCount] := Comp.Summary.ConstructCopy;
        end;
        Inc(RowCount);
      end;
    end;

    DtxTable.RowCount := RowCount;
    DtxTable.SaveDtx(ADtxWriter);
  finally
    DtxTable.Free;
  end;
end;

procedure TGroupDtxProducer.SaveGroupInfoComponentList(AGroupInfo: TGroupInfo;
  ADtxWriter: TDtxWriter);
var
  RowCount: Integer;
  I: Integer;
  Comp: TComponentInfo;
  DtxTable: TDtxTable;
  SymbolList: TSymbolList;
begin
  if CountComp(AGroupInfo) = 0 then
    Exit;
  RowCount := 0;

  DtxTable := TDtxTable.Create;
  try
    DtxTable.ColCount := 2;
    DtxTable.RowCount := AGroupInfo.Components.Count;
    DtxTable.Header[0] := 'Component';
    DtxTable.Header[1] := 'Description';
    DtxTable.HasHeader := True;

    for I := 0 to AGroupInfo.Components.Count - 1 do
    begin
      Comp := ComponentInfos[ComponentInfos.IndexOf(AGroupInfo.Components[I])];
      if cifComponent in Comp.Flags then
      begin
        if IsNotDocumented(Comp.Summary) then
        begin
          DtxTable.Cells[0, RowCount] := Format('<IMAGE %s> %s', [Comp.Image, Comp.Name]);
          SymbolList := TSymbolList.Create;
          try
            SymbolList.Parse(Format('Not documented (' + cEditLink + ').', [Comp.Name, 'Edit topic']));
            DtxTable.CellsSymbols[1, RowCount] := SymbolList;
          except
            SymbolList.Free;
            raise;
          end;
        end
        else
        begin
          if NoLinks then
            DtxTable.Cells[0, RowCount] := Format('<IMAGE %s> %s', [Comp.Image, Comp.Name])
          else
            DtxTable.Cells[0, RowCount] := Format('<IMAGE %s> <LINK %s>', [Comp.Image, Comp.Name]);
          DtxTable.CellsSymbols[1, RowCount] := Comp.Summary.ConstructCopy;
        end;
        Inc(RowCount);
      end;
    end;

    DtxTable.RowCount := RowCount;
    DtxTable.SaveDtx(ADtxWriter);
  finally
    DtxTable.Free;
  end;
end;

procedure TGroupDtxProducer.SaveGroupInfoDescription(AGroupInfo: TGroupInfo;
  ADtxWriter: TDtxWriter);
begin
  ADtxWriter.WriteWrappedData(AGroupInfo.GroupText);
  ADtxWriter.WriteLn;
  if AGroupInfo.Components.Count > 0 then
  begin
    if CountComp(AGroupInfo) > 0 then
      ADtxWriter.WriteWrappedData('This group contains the following components and controls:');
  end
  else
    ADtxWriter.WriteWrappedData('This group is currently empty.');
end;

procedure TGroupDtxProducer.SaveGroupInfoRoutineList(AGroupInfo: TGroupInfo;
  ADtxWriter: TDtxWriter);
var
  I: Integer;
  Comp: TComponentInfo;
  DtxTable: TDtxTable;
  RowCount: Integer;
  HasClasses: Boolean;
  HasComps: Boolean;
  SymbolList: TSymbolList;
begin
  if CountRoutines(AGroupInfo) = 0 then
    Exit;

  HasClasses := CountClass(AGroupInfo) > 0;
  HasComps := CountComp(AGroupInfo) > 0;

  if HasComps then
  begin
    ADtxWriter.WriteLn;
    ADtxWriter.WriteLn;
    if HasClasses then
      ADtxWriter.WriteWrappedData(
        'In addition to these components and classes, the following routines belong to this group:')
    else
      ADtxWriter.WriteWrappedData(
        'In addition to these components, the following routines belong to this group:')
  end
  else
    if HasClasses then
  begin
    ADtxWriter.WriteLn;
    ADtxWriter.WriteLn;
    ADtxWriter.WriteWrappedData('In addition to these classes, the following routines belong to this group:');
  end
  else
    ADtxWriter.WriteWrappedData('This group contains the following classes:');
  RowCount := 0;

  DtxTable := TDtxTable.Create;
  try
    DtxTable.ColCount := 2;
    DtxTable.RowCount := AGroupInfo.Components.Count;
    DtxTable.Header[0] := 'Routine';
    DtxTable.Header[1] := 'Description';
    DtxTable.HasHeader := True;

    for I := 0 to AGroupInfo.Components.Count - 1 do
    begin
      Comp := ComponentInfos[ComponentInfos.IndexOf(AGroupInfo.Components[I])];
      if cifRoutine in Comp.Flags then
      begin
        if IsNotDocumented(Comp.Summary) then
        begin
          DtxTable.Cells[0, RowCount] := Comp.Name;
          SymbolList := TSymbolList.Create;
          SymbolList.Parse(Format('Not documented (' + cEditLink + ').', [Comp.Name, 'Edit topic']));

          DtxTable.CellsSymbols[1, RowCount] := SymbolList;
        end
        else
        begin
          if NoLinks then
            DtxTable.Cells[0, RowCount] := Comp.Name else
            DtxTable.Cells[0, RowCount] := LinkStr(Comp.Name);
          DtxTable.CellsSymbols[1, RowCount] := Comp.Summary.ConstructCopy;
        end;
        Inc(RowCount);
      end;
    end;

    DtxTable.RowCount := RowCount;
    DtxTable.SaveDtx(ADtxWriter);
  finally
    DtxTable.Free;
  end;
end;

procedure TGroupDtxProducer.SaveGroupInfos(AGroupInfos: TGroupInfos;
  ADtxWriter: TDtxWriter);
var
  I: Integer;
begin
  with AGroupInfos do
    for I := 0 to Count - 1 do
      SaveGroupInfo(Items[I], ADtxWriter, -Count + I);
end;

end.

