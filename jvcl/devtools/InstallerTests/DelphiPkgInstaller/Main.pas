unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, jpeg, ExtCtrls, JvWizard, JvWizardRouteMapSteps, JvExControls,
  Packages, JvComponent, VirtualTrees, JvTabBar, JvPageList, ImgList,
  JvAppInst, XPMan, StdCtrls, Mask, JvExMask, JvToolEdit, CheckLst,
  ComCtrls, JvExComCtrls, JvProgressBar, Logging, Menus;

const
  WM_STARTINSTALL = WM_USER + 1;

type
  TFormMain = class(TForm)
    JvWizard: TJvWizard;
    JvWizardComponents: TJvWizardInteriorPage;
    JvWizardRouteMapSteps: TJvWizardRouteMapSteps;
    JvWizardPageChooseDirectories: TJvWizardInteriorPage;
    JvTabBar: TJvTabBar;
    JvPageList: TJvPageList;
    PageDesigntime: TJvStandardPage;
    VTreeComps: TVirtualStringTree;
    PageRuntime: TJvStandardPage;
    VTreeRun: TVirtualStringTree;
    ImageListPackages: TImageList;
    VTreeDesign: TVirtualStringTree;
    PanelDesignSplit: TPanel;
    VTreeUnits: TVirtualStringTree;
    PanelRunSplit: TPanel;
    PageComponents: TJvStandardPage;
    JvAppInstances: TJvAppInstances;
    JvModernTabBarPainter1: TJvModernTabBarPainter;
    VTreePalette: TVirtualStringTree;
    EditBPLDirectory: TJvDirectoryEdit;
    EditInstallDir: TJvDirectoryEdit;
    Label1: TLabel;
    Label2: TLabel;
    XPManifest1: TXPManifest;
    Label3: TLabel;
    JvWizardPageInstall: TJvWizardInteriorPage;
    Image: TImage;
    LblVersion: TLabel;
    ProgressBar: TJvProgressBar;
    LblStatus: TLabel;
    PopupMenu: TPopupMenu;
    MenuInstallAll: TMenuItem;
    MenuInstallNone: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure VTreeDesignInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VTreeDesignFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTreeDesignGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure VTreeDesignGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VTreeDesignChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VTreeDesignChange(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure VTreeCompsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VTreeCompsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure VTreeCompsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VTreeRunChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VTreeUnitsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VTreeUnitsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure VTreeUnitsGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VTreeUnitsInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VTreeUnitsMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VTreeCompsMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VTreeUnitsBeforeCellPaint(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      CellRect: TRect);
    procedure VTreeUnitsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType);
    procedure VTreePaletteInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    procedure VTreePaletteGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: WideString);
    procedure VTreePaletteGetImageIndex(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: Integer);
    procedure VTreePaletteMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure VTreePaletteInitChildren(Sender: TBaseVirtualTree;
      Node: PVirtualNode; var ChildCount: Cardinal);
    procedure VTreePaletteChecked(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure JvWizardCancelButtonClick(Sender: TObject);
    procedure JvWizardActivePageChanged(Sender: TObject);
    procedure JvWizardPageInstallEnterPage(Sender: TObject;
      const FromPage: TJvWizardCustomPage);
    procedure JvWizardPageInstallCancelButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure JvWizardPageInstallFinishButtonClick(Sender: TObject;
      var Stop: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure MenuInstallNoneClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FUpdateCheckBoxesLock: Boolean;
    FAborted: Boolean;
    FInstalling: Boolean;
    procedure UpdateCheckBoxes;
    procedure UpdatePackageSelection;
    procedure UpdatePaletteSelection;
    procedure CopyOneFile(const SourceFilename, DestFilename: string);
    procedure WMStartInstall(var Msg: TMessage); message WM_STARTINSTALL;
    procedure RegisterToIDE(Log: TLog);
    procedure MakeDirectories(Log: TLog; const Dir: string);
  public
    { Public-Deklarationen }
  end;

var
  FormMain: TFormMain;

implementation

uses
  JvGnugettext, DataModuleMain, Configuration, Utils, DelphiData;

{$R *.dfm}

type
  TDataKind = (dkPackage, dkComponent);

  PDataP = ^TDataP;
  TDataP = record
    Package: IPackage;
  end;

  PDataC = ^TDataC;
  TDataC = record
    Comp: IComponentItem;
  end;

  PDataU = ^TDataU;
  TDataU = record
    UnitItem: IUnit;
  end;

  PDataPC = ^TDataPC;
  TDataPC = record
    Palette: string;
    Comp: IComponentItem;
  end;

procedure TFormMain.UpdateCheckBoxes;
begin
  if FUpdateCheckBoxesLock then
    Exit;
  FUpdateCheckBoxesLock := True;
  try
    UpdatePackageSelection;
    UpdatePaletteSelection;
  finally
    FUpdateCheckBoxesLock := False;
  end;
end;

procedure TFormMain.UpdatePackageSelection;
var
  Node: PVirtualNode;
begin
  Node := VTreeRun.GetFirst;
  while Node <> nil do
  begin
    if PDataP(VTreeRun.GetNodeData(Node)).Package.Checked then
      VTreeRun.CheckState[Node] := csCheckedNormal
    else
      VTreeRun.CheckState[Node] := csUncheckedNormal;
    Node := VTreeRun.GetNextSibling(Node);
  end;

  Node := VTreeDesign.GetFirst;
  while Node <> nil do
  begin
    if PDataP(VTreeDesign.GetNodeData(Node)).Package.Checked then
      VTreeDesign.CheckState[Node] := csCheckedNormal
    else
      VTreeDesign.CheckState[Node] := csUncheckedNormal;
    Node := VTreeDesign.GetNextSibling(Node);
  end;
end;

procedure TFormMain.UpdatePaletteSelection;
var
  Node, Child: PVirtualNode;
  Data: PDataPC;
begin
  Node := VTreePalette.GetFirst;
  while Node <> nil do
  begin
    Child := VTreePalette.GetFirstChild(Node);
    while Child <> nil do
    begin
      Data := PDataPC(VTreePalette.GetNodeData(Child));
      if Data.Comp.Checked then
        VTreePalette.CheckState[Child] := csCheckedNormal
      else
        VTreePalette.CheckState[Child] := csUncheckedNormal;
      Child := VTreePalette.GetNextSibling(Child);
    end;
    Node := VTreePalette.GetNextSibling(Node);
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
var
  i, k: Integer;
  Pals: TStringList;
  Filename: string;
begin
  JvPageList.ActivePageIndex := 0;

  if FileExists('Config\' + Config.WizardPicture) then
    JvWizardRouteMapSteps.Image.Picture.LoadFromFile('Config\' + Config.WizardPicture);

  Filename := Config.WelcomePicture;
  if (Filename <> '') and (Filename[1] = '*') then
  begin
    Image.Proportional := True;
    Image.Stretch := True;
    Delete(Filename, 1, 1);
  end;
  if FileExists('Config\' + Filename) then
    Image.Picture.LoadFromFile('Config\' + Filename);

  EditInstallDir.Text := ResolveDirectory(Config.DefaultInstallDir);
  EditBPLDirectory.Text := ResolveDirectory(Config.DefaultBPLDir);
  if Config.Title <> '' then
    Caption := Config.Title;

  LblVersion.Caption := Config.Target.DisplayName;

  VTreeDesign.NodeDataSize := SizeOf(TDataP);
  VTreeRun.NodeDataSize := SizeOf(TDataP);
  VTreeDesign.RootNodeCount := 0;
  VTreeRun.RootNodeCount := 0;

  for i := 0 to PackageList.PackageCount - 1 do
  begin
    if PackageList.Packages[i].IsRunOnly then
      PDataP(VTreeRun.GetNodeData(VTreeRun.AddChild(nil))).Package := PackageList.Packages[i]
    else
      PDataP(VTreeDesign.GetNodeData(VTreeDesign.AddChild(nil))).Package := PackageList.Packages[i];
    for k := 0 to Config.Target.KnownPackages.Count - 1 do
      if SameText(ChangeFileExt(Config.Target.KnownPackages[k].Name, ''), PackageList.Packages[i].Name) then
        PackageList.Packages[i].Checked := True;
  end;

  VTreeComps.NodeDataSize := SizeOf(TDataC);
  VTreeUnits.NodeDataSize := SizeOf(TDataU);

  VTreePalette.RootNodeCount := 0;
  VTreePalette.NodeDataSize := SizeOf(TDataPC);
  Pals := TStringList.Create;
  try
    Pals.Sorted := True;
    for i := 0 to PackageList.ComponentCount - 1 do
    begin
      if Pals.IndexOf(PackageList.Components[i].Palette) < 0 then
      begin
        Pals.Add(PackageList.Components[i].Palette);
        PDataPC(VTreePalette.GetNodeData(VTreePalette.AddChild(nil))).Palette := PackageList.Components[i].Palette;
      end;
    end;
  finally
    Pals.Free;
  end;

  if VTreePalette.RootNodeCount = 0 then
  begin
    JvTabBar.Tabs[0].Visible := False;
    JvPageList.ActivePageIndex := 1;
  end;

  if VTreeDesign.RootNodeCount = 0 then
  begin
    JvTabBar.Tabs[1].Visible := False;
    JvPageList.ActivePageIndex := 2;
  end;

  UpdateCheckBoxes;
end;

procedure TFormMain.VTreeDesignInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Sender.CheckType[Node] := ctCheckBox;
  if PDataP(Sender.GetNodeData(Node)).Package.Checked then
    Sender.CheckState[Node] := csCheckedNormal
  else
    Sender.CheckState[Node] := csUncheckedNormal;
end;

procedure TFormMain.VTreeDesignFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
begin
  PDataP(Sender.GetNodeData(Node)).Package := nil;
end;

procedure TFormMain.VTreeDesignGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  if PDataP(Sender.GetNodeData(Node)).Package.Description = '' then
    CellText := PDataP(Sender.GetNodeData(Node)).Package.Name
  else
    CellText := PDataP(Sender.GetNodeData(Node)).Package.Description;  
end;

procedure TFormMain.VTreeDesignGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if PDataP(Sender.GetNodeData(Node)).Package.IsRunOnly then
    ImageIndex := 2
  else
    ImageIndex := 0;
end;

procedure TFormMain.VTreeDesignChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Package: IPackage;
begin
  if FUpdateCheckBoxesLock then
    Exit;
  Package := PDataP(Sender.GetNodeData(Node)).Package;
  Package.Checked := Node.CheckState = csCheckedNormal;
  UpdateCheckBoxes;
end;

procedure TFormMain.VTreeDesignChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VTreeComps.Clear;
  if Node <> nil then
    VTreeComps.RootNodeCount := PDataP(Sender.GetNodeData(Node)).Package.ComponentCount;
end;

procedure TFormMain.VTreeCompsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PDataC;
begin
  Data := Sender.GetNodeData(Node);
  Data.Comp := PDataP(VTreeDesign.GetNodeData(VTreeDesign.GetFirstSelected)).Package.Components[Node.Index];
end;

procedure TFormMain.VTreeCompsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data: PDataC;
begin
  CellText := '';
  Data := Sender.GetNodeData(Node);
  if Column = 0 then
    CellText := Data.Comp.ComponentClass
  else if Column = 1 then
    CellText := Data.Comp.Palette;
end;

procedure TFormMain.VTreeCompsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Column = 0 then
    ImageIndex := PDataC(Sender.GetNodeData(Node)).Comp.ImageIndex;
end;

procedure TFormMain.VTreeRunChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  VTreeUnits.Clear;
  if Node <> nil then
    VTreeUnits.RootNodeCount := PDataP(Sender.GetNodeData(Node)).Package.UnitCount;
end;

procedure TFormMain.VTreeUnitsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data: PDataU;
begin
  if ParentNode = nil then
  begin
    Data := Sender.GetNodeData(Node);
    Data.UnitItem := PDataP(VTreeRun.GetNodeData(VTreeRun.GetFirstSelected)).Package.Units[Node.Index];
    if Data.UnitItem.ComponentCount > 0 then
      InitialStates := InitialStates + [ivsHasChildren, ivsExpanded];
  end
  else
  begin
    Data := Sender.GetNodeData(ParentNode);
    PDataC(Sender.GetNodeData(Node)).Comp := Data.UnitItem.Components[Node.Index];
  end;
end;

procedure TFormMain.VTreeUnitsGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
begin
  CellText := '';
  if Sender.NodeParent[Node] = nil then
    CellText := PDataU(Sender.GetNodeData(Node)).UnitItem.Name
  else
    CellText := PDataC(Sender.GetNodeData(Node)).Comp.ComponentClass;
end;

procedure TFormMain.VTreeUnitsGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
begin
  if Sender.NodeParent[Node] <> nil then
    ImageIndex := PDataC(Sender.GetNodeData(Node)).Comp.ImageIndex;
end;

procedure TFormMain.VTreeUnitsInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
begin
  ChildCount := PDataU(Sender.GetNodeData(Node)).UnitItem.ComponentCount;
end;

procedure TFormMain.VTreeUnitsMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  if Sender.NodeParent[Node] <> nil then
    NodeHeight := DMMain.ImageListComponents.Height + 2;
end;

procedure TFormMain.VTreeCompsMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
begin
  NodeHeight := DMMain.ImageListComponents.Height + 2;
end;

procedure TFormMain.VTreeUnitsBeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellRect: TRect);
begin
  if Sender.NodeParent[Node] <> nil then
  begin
    TargetCanvas.Brush.Color := $fff5f5;
    TargetCanvas.FillRect(CellRect);
  end;
end;

procedure TFormMain.VTreeUnitsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if Sender.NodeParent[Node] = nil then
    TargetCanvas.Font.Style := [fsBold];
end;

procedure TFormMain.VTreePaletteInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: PDataPC;
  i: Integer;
  Index: Integer;
begin
  Data := Sender.GetNodeData(Node);
  if ParentNode = nil then
  begin
    InitialStates := InitialStates + [ivsHasChildren];
    Sender.CheckType[Node] := ctTriStateCheckBox;
  end
  else
  begin
    ParentData := Sender.GetNodeData(ParentNode);
    Index := Node.Index;
    for i := 0 to PackageList.ComponentCount - 1 do
      if CompareText(PackageList.Components[i].Palette, ParentData.Palette) = 0 then
      begin
        if Index = 0 then
        begin
          Data.Comp := PackageList.Components[i];
          Break;
        end;
        Dec(Index);
      end;
    Sender.CheckType[Node] := ctCheckBox;
    if Data.Comp.Checked then
      Sender.CheckState[Node] := csCheckedNormal
    else;
      Sender.CheckState[Node] := csUncheckedNormal;
  end;
end;

procedure TFormMain.VTreePaletteGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: WideString);
var
  Data: PDataPC;
begin
  CellText := '';
  Data := Sender.GetNodeData(Node);
  if Data.Comp = nil then
  begin
    if Column = 0 then
      CellText := Data.Palette;
  end
  else
  begin
    if Column = 0 then
      CellText := Data.Comp.ComponentClass
    else if Column = 1 then
      CellText := Data.Comp.UnitName
    else if Column = 2 then
      CellText := Data.Comp.Package.Name;
  end;
end;

procedure TFormMain.VTreePaletteGetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: Boolean; var ImageIndex: Integer);
var
  Data: PDataPC;
begin
  Data := Sender.GetNodeData(Node);
  if Data.Comp <> nil then
  begin
    if Column = 0 then
      ImageIndex := Data.Comp.ImageIndex;
  end;
end;

procedure TFormMain.VTreePaletteMeasureItem(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
var
  Data: PDataPC;
begin
  Data := Sender.GetNodeData(Node);
  if Data.Comp <> nil then
    NodeHeight := DMMain.ImageListComponents.Height + 2;
end;

procedure TFormMain.VTreePaletteInitChildren(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var ChildCount: Cardinal);
var
  Data: PDataPC;
  i: Integer;
begin
  Data := Sender.GetNodeData(Node);
  if Data.Comp = nil then
  begin
    for i := 0 to PackageList.ComponentCount - 1 do
      if CompareText(PackageList.Components[i].Palette, Data.Palette) = 0 then
        Inc(ChildCount);
  end;
end;

procedure TFormMain.VTreePaletteChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  Data: PDataPC;
  i: Integer;
begin
  if FUpdateCheckBoxesLock then
    Exit;
  Data := Sender.GetNodeData(Node);
  if Data.Comp <> nil then
    Data.Comp.Checked := Node.CheckState = csCheckedNormal
  else
  begin
    if not Sender.ChildrenInitialized[Node] then
    begin
      for i := 0 to PackageList.ComponentCount - 1 do
        if CompareText(PackageList.Components[i].Palette, Data.Palette) = 0 then
          PackageList.Components[i].Checked := Node.CheckState = csCheckedNormal;
    end;
  end;
  UpdateCheckBoxes;
end;

procedure TFormMain.JvWizardCancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.JvWizardActivePageChanged(Sender: TObject);
begin
  if JvWizard.ActivePageIndex = 1 then
  begin
    JvWizard.ButtonNext.Caption := _('&Install');
  end
  else
  begin
    JvWizard.ButtonNext.Caption := _('&Next >');
  end;
end;

procedure TFormMain.JvWizardPageInstallCancelButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  Stop := True;
  FAborted := True;
end;

procedure TFormMain.CopyOneFile(const SourceFilename, DestFilename: string);
var
  InFile, OutFile: TFileStream;
  CreationTime, LastWriteTime, LastAccessTime: TFileTime;
  Buf: array[0..128 * 1024] of Byte;
  StreamSize, Readn: Integer;
begin
  InFile := TFileStream.Create(SourceFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutFile := TFileStream.Create(DestFileName, fmCreate or fmShareExclusive);
    try
      StreamSize := InFile.Size;
      while StreamSize > 0 do
      begin
        Readn := StreamSize;
        if Readn > Length(Buf) then
          Readn := Length(Buf);
        InFile.Read(Buf[0], Readn);
        OutFile.Write(Buf[0], Readn);
        Dec(StreamSize, Readn);
        if FAborted then
          Break;
      end;
      GetFileTime(InFile.Handle, @CreationTime, @LastAccessTime, @LastWriteTime);
      SetFileTime(OutFile.Handle, @CreationTime, @LastAccessTime, @LastWriteTime);
    finally
      OutFile.Free;
    end;
  finally
    InFile.Free;
  end;
  SetFileAttributes(PChar(DestFileName), GetFileAttributes(PChar(SourceFileName)));
end;

procedure TFormMain.JvWizardPageInstallEnterPage(Sender: TObject;
  const FromPage: TJvWizardCustomPage);
begin
  FInstalling := True;
  PostMessage(Handle, WM_STARTINSTALL, 0, 0);
end;

procedure TFormMain.JvWizardPageInstallFinishButtonClick(Sender: TObject;
  var Stop: Boolean);
begin
  Close;
end;

procedure TFormMain.MakeDirectories(Log: TLog; const Dir: string);
begin
  if (Dir <> '') and (Length(Dir) > 3) and not DirectoryExists(Dir) then
  begin
    MakeDirectories(Log, ExtractFileDir(Dir));
    CreateDir(Dir);
    Log.DirAdd(Dir);
  end;
end;

function IsFileNewerThan(const Filename, CompareToFile: string): Boolean;
var
  sr1, sr2: TSearchRec;
begin
  Result := True;
  if (FindFirst(Filename, faAnyFile and not faDirectory, sr1) = 0) and
     (FindFirst(CompareToFile, faAnyFile and not faDirectory, sr2) = 0) then
    Result := (sr1.Time > sr2.Time) or (sr1.Size <> sr2.Size);
end;

procedure TFormMain.WMStartInstall(var Msg: TMessage);
var
  Files, Packages: TStringList;
  i: Integer;
  Size, CopiedSize: Int64;
  Percentage, NewPercentage: Integer;
  DestFilename, Filename: string;
  DataDir: string;
  PackagesDir: string;
  Log: TLog;
begin
  LblStatus.Caption := 'Collecting files...';
  Application.ProcessMessages;
  try
    Config.InstallDir := ExcludeTrailingPathDelimiter(EditInstallDir.Text);
    Config.BplDir := ExcludeTrailingPathDelimiter(EditBPLDirectory.Text);

    Log := TLog.Create;
    FAborted := False;
    try
      // start Installation
      Files := TStringList.Create;
      Packages := TStringList.Create;
      try
        DataDir := ExtractFilePath(ParamStr(0)) + 'Data';
        PackagesDir := ExtractFilePath(ParamStr(0)) + 'Packages';
        FindFiles(DataDir, '*.*', True, Files, []);
        FindFiles(PackagesDir, '*.*', False, Packages, []);

        Size := 0;
        for i := 0 to Files.Count - 1 do
          Inc(Size, Integer(Files.Objects[i]));
        for i := 0 to Packages.Count - 1 do
          Inc(Size, Integer(Packages.Objects[i]));

        ProgressBar.Max := 100;
        ProgressBar.Position := 0;
        Percentage := 0;
        CopiedSize := 0;
        for i := 0 to Files.Count - 1 do
        begin
          DestFilename := Config.InstallDir + Copy(Files[i], Length(DataDir) + 1, MaxInt);
          MakeDirectories(Log, ExtractFileDir(DestFilename));
          if not FileExists(DestFileName) or IsFileNewerThan(Files[i], DestFilename) then
          begin
            LblStatus.Caption := Format(_('Copying file: %s'), [Copy(Files[i], Length(DataDir) + 1 + 1, MaxInt)]);
            LblStatus.Update;
            CopyOneFile(Files[i], DestFilename);
            Log.FileAdd(DestFilename);
          end;

          { progress }
          Inc(CopiedSize, Integer(Files.Objects[i]));
          NewPercentage := CopiedSize * 100 div Size;
          if NewPercentage <> Percentage then
          begin
            Percentage := NewPercentage;
            ProgressBar.Position := Percentage;
            Application.ProcessMessages;
          end;
          if FAborted then
            Break;
        end;

        MakeDirectories(Log, Config.BplDir);
        for i := 0 to Packages.Count - 1 do
        begin
          DestFilename := Config.BplDir + Copy(Packages[i], Length(PackagesDir) + 1, MaxInt);
          if FileAge(Packages[i]) > FileAge(DestFilename) then
          begin
            LblStatus.Caption := Format(_('Copying file: %s'), [Copy(Packages[i], Length(PackagesDir) + 1 + 1, MaxInt)]);
            LblStatus.Update;
            CopyOneFile(Packages[i], DestFilename);
            Log.FileAdd(DestFilename);
          end;

          { progress }
          Inc(CopiedSize, Integer(Packages.Objects[i]));
          NewPercentage := CopiedSize * 100 div Size;
          if NewPercentage <> Percentage then
          begin
            Percentage := NewPercentage;
            ProgressBar.Position := Percentage;
            Application.ProcessMessages;
          end;
          if FAborted then
            Break;
        end;
      finally
        Packages.Free;
        Files.Free;
      end;

      LblStatus.Caption := 'Registering packages...';
      Application.ProcessMessages;
      RegisterToIDE(Log);
    finally
      try
        LblStatus.Caption := 'Generating uninstall information...';
        Application.ProcessMessages;
        Filename := ExtractFilePath(ParamStr(0)) + 'Config\DelphiPkgUninstall.bin';
        DestFilename := Config.InstallDir + PathDelim + 'DelphiPkgUninstall.exe';
        if FileExists(Filename) then
          CopyOneFile(Filename, DestFilename);
        try
          Log.SaveToFile(Config.InstallDir + PathDelim + 'install.log');
        finally
          Log.Free;
        end;
      finally
        LblStatus.Caption := 'Finished.';
        Application.ProcessMessages;
        FInstalling := False;
        JvWizardPageInstall.VisibleButtons := [bkFinish];
      end;
    end;
  except
    LblStatus.Caption := 'Failed.';
    JvWizardPageInstall.VisibleButtons := [bkFinish];
    Application.HandleException(Self);
  end;
end;

procedure TFormMain.RegisterToIDE(Log: TLog);
var
  Dir, Action, Kind: string;
  i: Integer;
  List: TStrings;
  Filename: string;
begin
  for i := 0 to Config.LibraryPaths.Count - 1 do
  begin
    Dir := Config.LibraryPaths[i];
    if Trim(Dir) = '' then
      Continue;

    Action := Copy(Dir, 1, Pos('=', Dir) - 1);
    Delete(Dir, 1, Length(Action) + 1);
    Kind := Copy(Dir, 1, Pos(',', Dir) - 1);
    Delete(Dir, 1, Length(Kind) + 1);

    Dir := ResolveDirectory(Dir);

    if SameText(Kind, 'Search') then
      List := Config.Target.SearchPaths
    else if SameText(Kind, 'Browse') then
      List := Config.Target.BrowsingPaths
    else if SameText(Kind, 'Debug') then
      List := Config.Target.DebugDcuPaths
    else
      raise Exception.CreateFmt('Invalid config.ini file: Unknown LibraryPath kind "%s"', [Kind]);

    Dir := Config.Target.InsertDirMacros(Dir);
    if (List.IndexOf(Dir) < 0) and (List.IndexOf(Config.Target.ExpandDirMacros(Dir)) < 0) then
      List.Add(Dir);
    Log.PathListAdd(Kind, Dir);
  end;

  for i := 0 to PackageList.PackageCount - 1 do
  begin
    if not PackageList.Packages[i].IsRunOnly then
    begin
      if (Config.Target.DisabledPackages.IndexOfFilename(Config.Target.ExpandDirMacros(Filename)) >= 0) then
        Config.Target.DisabledPackages.Remove(Config.Target.ExpandDirMacros(Filename));
      if (Config.Target.DisabledPackages.IndexOfFilename(Filename) >= 0) then
        Config.Target.DisabledPackages.Remove(Filename);

      Filename := Config.Target.InsertDirMacros(Config.BplDir + PathDelim + PackageList.Packages[i].Name + '.bpl');
      if PackageList.Packages[i].Checked then
      begin
        if (Config.Target.KnownPackages.IndexOfFilename(Config.Target.ExpandDirMacros(Filename)) < 0) and
           (Config.Target.KnownPackages.IndexOfFilename(Filename) < 0) then
        begin
          Config.Target.KnownPackages.Add(Filename, PackageList.Packages[i].Description);
          Log.PackageAdd(Filename);
        end;
      end
      else
      begin
        if (Config.Target.KnownPackages.IndexOfFilename(Config.Target.ExpandDirMacros(Filename)) >= 0) then
          Config.Target.KnownPackages.Remove(Config.Target.ExpandDirMacros(Filename));
        if (Config.Target.KnownPackages.IndexOfFilename(Filename) >= 0) then
          Config.Target.KnownPackages.Remove(Filename);
      end;
    end;
  end;

  Config.Target.SavePaths;
  Config.Target.SavePackagesLists;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not FInstalling;
end;

procedure TFormMain.MenuInstallNoneClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to PackageList.PackageCount - 1 do
    PackageList.Packages[i].Checked := Sender = MenuInstallAll;
  UpdateCheckBoxes;
end;

end.
