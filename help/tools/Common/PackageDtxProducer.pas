unit PackageDtxProducer;

interface

uses
  Classes, ContNrs, JVCLHelpUtils, DtxParser, DtxAnalyzer;

type
  TPackageDtxProducer = class(TTask)
  private
    FPackageInfos: TDtxPackages;
    FPackagesDtxFileName: string;
    FPackageDescFileName: string;

    function IsNotDocumented(ASymbolList: TSymbolList): Boolean;
  protected
    function CountComp(APackageInfo: TDtxPackage): Integer;
    function CountClass(APackageInfo: TDtxPackage): Integer;
    function CountRoutines(APackageInfo: TDtxPackage): Integer;

    procedure SavePackageInfoDescription(APackageInfo: TDtxPackage; ADtxWriter: TDtxWriter);
    procedure SavePackageInfoComponentList(APackageInfo: TDtxPackage; ADtxWriter: TDtxWriter);
    procedure SavePackageInfoClassList(APackageInfo: TDtxPackage; ADtxWriter: TDtxWriter);
    procedure SavePackageInfoRoutineList(APackageInfo: TDtxPackage; ADtxWriter: TDtxWriter);
    procedure SavePackageInfo(APackageInfo: TDtxPackage; ADtxWriter: TDtxWriter; const TopicOrder: Integer);
    procedure SavePackageInfos(APackageInfos: TDtxPackages; ADtxWriter: TDtxWriter);
    procedure SaveDtx(ADtxWriter: TDtxWriter);
    procedure ReadPackageDescFileName;

    function CanStart: Boolean; override;
    function DoExecute: Boolean; override;
    function GetTaskDescription: string; override;
  public
    property PackageInfos: TDtxPackages read FPackageInfos write FPackageInfos;
    property PackagesDtxFileName: string read FPackagesDtxFileName write FPackagesDtxFileName;
    property PackageDescFileName: string read FPackageDescFileName write FPackageDescFileName;
  end;

implementation

uses
  SysUtils, ParserTypes;

function TPackageDtxProducer.CanStart: Boolean;
begin
  Result := not IsNullStr(PackagesDtxFileName) and Assigned(PackageInfos) and
    CheckFile(PackageDescFileName);
end;

function TPackageDtxProducer.CountClass(APackageInfo: TDtxPackage): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := APackageInfo.ComponentCount - 1 downto 0 do
    if cifClass in APackageInfo.ComponentInfos[I].Flags then
      Inc(Result);
end;

function TPackageDtxProducer.CountComp(APackageInfo: TDtxPackage): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := APackageInfo.ComponentCount - 1 downto 0 do
    if cifComponent in APackageInfo.ComponentInfos[I].Flags then
      Inc(Result);
end;

function TPackageDtxProducer.CountRoutines(APackageInfo: TDtxPackage): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := APackageInfo.ComponentCount - 1 downto 0 do
    if cifRoutine in APackageInfo.ComponentInfos[I].Flags then
      Inc(Result);
end;

function TPackageDtxProducer.DoExecute: Boolean;
var
  Stream: TFileStream;
  DtxWriter: TDtxWriter;
begin
  Result := True;

  ReadPackageDescFileName;

  Stream := TFileStream.Create(PackagesDtxFileName, fmCreate);
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

function TPackageDtxProducer.GetTaskDescription: string;
begin
  Result := 'Generating package dtx';
end;

function TPackageDtxProducer.IsNotDocumented(
  ASymbolList: TSymbolList): Boolean;
begin
  Result := Assigned(ASymbolList) and (ASymbolList.Count = 1) and
    (ASymbolList[0] is TStringSymbol) and
    (SameText(TStringSymbol(ASymbolList[0]).Value, cSummaryDefaultTextForBuild));
end;

procedure TPackageDtxProducer.ReadPackageDescFileName;
var
  Packages: TStringList;
  I: Integer;
  Index: Integer;
begin
  Packages := TStringLIst.Create;
  try
    Packages.LoadFromFile(PackageDescFileName);
    for I := 0 to FPackageInfos.Count - 1 do
    begin
      Index := Packages.IndexOfName(FPackageInfos[i].PackageName);
      if Index >= 0 then
        FPackageInfos[i].PackageText := Packages.ValueFromIndex[Index]
      else
      begin
        WarningMsgFmt('No description for package %s', [FPackageInfos[i].PackageName]);
        FPackageInfos[i].PackageText := 'No description';
      end;
    end;
  finally
    Packages.Free;
  end;
end;

procedure TPackageDtxProducer.SaveDtx(ADtxWriter: TDtxWriter);
begin
  ADtxWriter.WriteLn(StringOfChar('#', 100));
  ADtxWriter.WriteComment(Pad(' JEDI-VCL help: Package include file.', 94, ' ') + ' ##');
  ADtxWriter.WriteComment(Pad(' Include file generated on ' +
    FormatDateTime('dd-mm-yyyy "at" hh:nn:ss', Now), 94, ' ') + ' ##');
  ADtxWriter.Writeln(StringOfChar('#', 100));
  ADtxWriter.WriteTopicName('$JVCL.Packages');
  ADtxWriter.WriteLn('<GROUP $JVCL>');
  ADtxWriter.WriteTitle('Packages');
  ADtxWriter.BeginSection('Description');
  ADtxWriter.WriteWrappedData(
    'Previous versions of JVCL had one run-time and one design-time package for '+
    'each version of supported compilers (i.e one pair for Delphi 5, another pair '+
    'for Delphi 6 etc). The current JVCL, however, is split into a larger set of '+
    'smaller packages, where each package contains logically related components.');
  ADtxWriter.WriteLn;
  ADtxWriter.WriteWrappedData(
    'Each run-time package also has a matching design-time package with the '+
    'registration code and any design-time editors required by the run-time '+
    'package.');
  ADtxWriter.EndSection;
  ADtxWriter.WriteSepLine;

  SavePackageInfos(FPackageInfos, ADtxWriter);
end;

procedure TPackageDtxProducer.SavePackageInfo(APackageInfo: TDtxPackage;
  ADtxWriter: TDtxWriter; const TopicOrder: Integer);
begin
  with APackageInfo do
  begin
    ADtxWriter.WriteTopicName('$JVCL.Packages.' + PackageName);
    ADtxWriter.WriteLn('<GROUP $JVCL.Packages>');
    ADtxWriter.WriteLnFmt('<TOPICORDER %d>', [TopicOrder]);
    ADtxWriter.WriteTitle(PackageName + ' package');
    ADtxWriter.BeginSection('Description');
    SavePackageInfoDescription(APackageInfo, ADtxWriter);
    SavePackageInfoComponentList(APackageInfo, ADtxWriter);
    SavePackageInfoClassList(APackageInfo, ADtxWriter);
    SavePackageInfoRoutineList(APackageInfo, ADtxWriter);
    ADtxWriter.EndSection;
    ADtxWriter.WriteLn;
    ADtxWriter.WriteSepLine;
  end;
end;

procedure TPackageDtxProducer.SavePackageInfoClassList(APackageInfo: TDtxPackage;
  ADtxWriter: TDtxWriter);
var
  I: Integer;
  Comp: TComponentInfo;
  DtxTable: TDtxTable;
  RowCount: Integer;
  SymbolList: TSymbolList;
begin
  if CountClass(APackageInfo) = 0 then
    Exit;
  if CountComp(APackageInfo) > 0 then
  begin
    ADtxWriter.WriteLn;
    ADtxWriter.WriteLn;
    ADtxWriter.WriteWrappedData('In addition to these components, the following classes belong to this package:');
  end
  else
    ADtxWriter.WriteWrappedData('This package contains the following classes:');

  RowCount := 0;

  DtxTable := TDtxTable.Create;
  try
    DtxTable.ColCount := 2;
    DtxTable.RowCount := APackageInfo.ComponentCount;
    DtxTable.Header[0] := 'Class';
    DtxTable.Header[1] := 'Description';
    DtxTable.HasHeader := True;

    for I := 0 to APackageInfo.ComponentCount - 1 do
    begin
      Comp := APackageInfo.ComponentInfos[I];
      if cifClass in Comp.Flags then
      begin
        DtxTable.Cells[0, RowCount] := LinkStr(Comp.Name);
        if IsNotDocumented(Comp.Summary) then
        begin
          SymbolList := TSymbolList.Create;
          SymbolList.Parse(Format('Not documented (' + cEditLink + ').', [Comp.Name, 'Edit topic']));

          DtxTable.CellsSymbols[1, RowCount] := SymbolList;
        end else
        DtxTable.CellsSymbols[1, RowCount] := Comp.Summary.ConstructCopy;
        Inc(RowCount);
      end;
    end;

    DtxTable.RowCount := RowCount;
    DtxTable.SaveDtx(ADtxWriter);
  finally
    DtxTable.Free;
  end;
end;

procedure TPackageDtxProducer.SavePackageInfoComponentList(APackageInfo: TDtxPackage;
  ADtxWriter: TDtxWriter);
var
  RowCount: Integer;
  I: Integer;
  Comp: TComponentInfo;
  DtxTable: TDtxTable;
  SymbolList: TSymboLList;
begin
  if CountComp(APackageInfo) = 0 then
    Exit;
  RowCount := 0;

  DtxTable := TDtxTable.Create;
  try
    DtxTable.ColCount := 2;
    DtxTable.RowCount := APackageInfo.ComponentCount;
    DtxTable.Header[0] := 'Component';
    DtxTable.Header[1] := 'Description';
    DtxTable.HasHeader := True;

    for I := 0 to APackageInfo.ComponentCount - 1 do
    begin
      Comp := APackageInfo.ComponentInfos[I];
      if cifComponent in Comp.Flags then
      begin
        DtxTable.Cells[0, RowCount] := Format('<IMAGE %s> <LINK %s>', [Comp.Image, Comp.Name]);
        if IsNotDocumented(Comp.Summary) then
        begin
          SymbolList := TSymbolList.Create;
          SymbolList.Parse(Format('Not documented (' + cEditLink + ').', [Comp.Name, 'Edit topic']));

          DtxTable.CellsSymbols[1, RowCount] := SymbolList;
        end
        else
          DtxTable.CellsSymbols[1, RowCount] := Comp.Summary.ConstructCopy;
        Inc(RowCount);
      end;
    end;

    DtxTable.RowCount := RowCount;
    DtxTable.SaveDtx(ADtxWriter);
  finally
    DtxTable.Free;
  end;
end;

procedure TPackageDtxProducer.SavePackageInfoDescription(APackageInfo: TDtxPackage;
  ADtxWriter: TDtxWriter);
begin
  ADtxWriter.WriteWrappedData(APackageInfo.PackageText);
  ADtxWriter.WriteLn;
  if APackageInfo.ComponentCount > 0 then
  begin
    if CountComp(APackageInfo) > 0 then
      ADtxWriter.WriteWrappedData('This package contains the following components and controls:');
  end
  else
    ADtxWriter.WriteWrappedData('This package is currently empty.');
end;

procedure TPackageDtxProducer.SavePackageInfoRoutineList(APackageInfo: TDtxPackage;
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
  if CountRoutines(APackageInfo) = 0 then
    Exit;

  HasClasses := CountClass(APackageInfo) > 0;
  HasComps := CountComp(APackageInfo) > 0;

  if HasComps then
  begin
    ADtxWriter.WriteLn;
    ADtxWriter.WriteLn;
    if HasClasses then
      ADtxWriter.WriteWrappedData(
        'In addition to these components and classes, the following routines belong to this package:')
    else
      ADtxWriter.WriteWrappedData(
        'In addition to these components, the following routines belong to this package:')
  end
  else
    if HasClasses then
  begin
    ADtxWriter.WriteLn;
    ADtxWriter.WriteLn;
    ADtxWriter.WriteWrappedData('In addition to these classes, the following routines belong to this package:');
  end
  else
    ADtxWriter.WriteWrappedData('This package contains the following classes:');
  RowCount := 0;

  DtxTable := TDtxTable.Create;
  try
    DtxTable.ColCount := 2;
    DtxTable.RowCount := APackageInfo.ComponentCount;
    DtxTable.Header[0] := 'Routine';
    DtxTable.Header[1] := 'Description';
    DtxTable.HasHeader := True;

    for I := 0 to APackageInfo.ComponentCount - 1 do
    begin
      Comp := APackageInfo.ComponentInfos[I];
      if cifRoutine in Comp.Flags then
      begin
        DtxTable.Cells[0, RowCount] := LinkStr(Comp.Name);
        if IsNotDocumented(Comp.Summary) then
        begin
          SymbolList := TSymbolList.Create;
          SymbolList.Parse(Format('Not documented (' + cEditLink + ').', [Comp.Name, 'Edit topic']));

          DtxTable.CellsSymbols[1, RowCount] := SymbolList;
        end else
        DtxTable.CellsSymbols[1, RowCount] := Comp.Summary.ConstructCopy;
        Inc(RowCount);
      end;
    end;

    DtxTable.RowCount := RowCount;
    DtxTable.SaveDtx(ADtxWriter);
  finally
    DtxTable.Free;
  end;
end;

procedure TPackageDtxProducer.SavePackageInfos(APackageInfos: TDtxPackages;
  ADtxWriter: TDtxWriter);
var
  I: Integer;
begin
  with APackageInfos do
    for I := 0 to Count - 1 do
      SavePackageInfo(Items[I], ADtxWriter, -Count + I);
end;

end.
