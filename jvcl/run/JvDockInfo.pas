{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDockInfo.pas, released on 2003-12-31.

The Initial Developer of the Original Code is luxiaoban.
Portions created by luxiaoban are Copyright (C) 2002,2003 luxiaoban.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-12-31

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvDockInfo;

interface

uses
  Windows, Controls, IniFiles, Registry, Classes, SysUtils, Forms, Messages,
  {$IFDEF USEJVCL}
  JvAppStorage,
  {$ENDIF USEJVCL}
  JvDockControlForm, JvDockSupportClass, JvDockSupportProc;

type
  TJvDockInfoTree = class;

  TJvDockFormStyle = (dsNormal, dsConjoin, dsTab, dsDockPanel);

  TJvDockInfoZone = class(TJvDockBaseZone)
  private
    FDockFormName: string;
    FParentName: string;
    FDockRect: TRect;
    FLastDockSiteName: string;
    FUnDockLeft: Integer;
    FUnDockTop: Integer;
    FLRDockWidth: Integer;
    FTBDockHeight: Integer;
    FUnDockWidth: Integer;
    FUnDockHeight: Integer;
    FVSPaneWidth: Integer;
    FVisible: Boolean;
    FBorderStyle: TBorderStyle;
    FFormStyle: TFormStyle;
    FWindowState: TWindowState;
    FCanDocked: Boolean;
    FEachOtherDocked: Boolean;
    FLeftDocked: Boolean;
    FTopDocked: Boolean;
    FRightDocked: Boolean;
    FBottomDocked: Boolean;
    FDockFormStyle: TJvDockFormStyle;
    FDockClientData: string;
    FDockControl: TWinControl;
    function GetChildControlCount: Integer;
  public
    procedure SetDockInfoFromControlToNode(Control: TControl); virtual;
    procedure SetDockInfoFromNodeToControl(Control: TControl); virtual;
    procedure SetDockInfoFromDockControlToNode(DockControl: TJvDockBaseControl); virtual;
    procedure SetDockInfoFromNodeToDockControl(DockControl: TJvDockBaseControl); virtual;

    property DockFormName: string read FDockFormName write FDockFormName;
    property ParentName: string read FParentName write FParentName;
    property DockRect: TRect read FDockRect write FDockRect;
    property LastDockSiteName: string read FLastDockSiteName write FLastDockSiteName;
    property UnDockLeft: Integer read FUnDockLeft write FUnDockLeft;
    property UnDockTop: Integer read FUnDockTop write FUnDockTop;
    property LRDockWidth: Integer read FLRDockWidth write FLRDockWidth;
    property TBDockHeight: Integer read FTBDockHeight write FTBDockHeight;
    property UnDockWidth: Integer read FUnDockWidth write FUnDockWidth;
    property UnDockHeight: Integer read FUnDockHeight write FUnDockHeight;
    property VSPaneWidth: Integer read FVSPaneWidth write FVSPaneWidth;
    property BorderStyle: TBorderStyle read FBorderStyle write FBorderStyle;
    property FormStyle: TFormStyle read FFormStyle write FFormStyle;
    property WindowState: TWindowState read FWindowState write FWindowState;
    property Visible: Boolean read FVisible write FVisible;
    property CanDocked: Boolean read FCanDocked write FCanDocked;
    property EachOtherDocked: Boolean read FEachOtherDocked write FEachOtherDocked;
    property LeftDocked: Boolean read FLeftDocked write FLeftDocked;
    property TopDocked: Boolean read FTopDocked write FTopDocked;
    property RightDocked: Boolean read FRightDocked write FRightDocked;
    property BottomDocked: Boolean read FBottomDocked write FBottomDocked;
    property DockFormStyle: TJvDockFormStyle read FDockFormStyle write FDockFormStyle;
    property DockClientData: string read FDockClientData write FDockClientData;
    property DockControl: TWinControl read FDockControl write FDockControl;
  end;

  TJvDockInfoStyle =
    (isNone, isJVCLReadInfo, isJVCLWriteInfo, isReadFileInfo, isWriteFileInfo, isReadRegInfo, isWriteRegInfo);

  TJvDockInfoTree = class(TJvDockBaseTree)
  private
    {$IFDEF USEJVCL}
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    {$ENDIF USEJVCL}
    FDockInfoIni: TCustomIniFile;
    FDockInfoReg: TRegistry;
    FRegName: string;
    FJvDockInfoStyle: TJvDockInfoStyle;
    FDataStream: TMemoryStream;
    function FindDockForm(FormName: string): TCustomForm;
    function CreateHostControl(ATreeZone: TJvDockInfoZone): TWinControl;
    {$IFDEF USEJVCL}
    function GetAppStoragePath: string;
    {$ENDIF}
  protected
    procedure ScanTreeZone(TreeZone: TJvDockBaseZone); override;
    {$IFDEF USEJVCL}
    procedure CreateZoneAndAddInfoFromAppStorage; virtual;
    {$ENDIF USEJVCL}
    procedure CreateZoneAndAddInfoFromIni; virtual;
    procedure CreateZoneAndAddInfoFromReg; virtual;
    procedure SetDockControlInfo(ATreeZone: TJvDockInfoZone); virtual;
  public
    constructor Create(TreeZone: TJvDockTreeZoneClass); override;
    destructor Destroy; override;
    procedure CreateZoneAndAddInfoFromApp(Control: TControl); virtual;

    {$IFDEF USEJVCL}
    procedure ReadInfoFromAppStorage;
    procedure WriteInfoToAppStorage;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read GetAppStoragePath write FAppStoragePath;
    {$ENDIF USEJVCL}
    procedure ReadInfoFromIni;
    procedure ReadInfoFromReg(RegName: string);
    procedure WriteInfoToIni;
    procedure WriteInfoToReg(RegName: string);
    property DockInfoIni: TCustomIniFile read FDockInfoIni write FDockInfoIni;
    property DockInfoReg: TRegistry read FDockInfoReg write FDockInfoReg;
  end;

implementation

uses
  JvDockGlobals, JvDockVSNetStyle;

function FindDockForm(FormName: string): TCustomForm;
begin
  if Pos(RsDockJvDockInfoSplitter, FormName) > 0 then
    Result := nil
  else
    Result := JvDockFindDockFormWithName(FormName);
end;

function FindDockPanel(ControlName: string): TWinControl;
var
  Index: Word;
  DockServer: TJvDockServer;
begin
  Result := nil;
  Index := Pos(RsDockJvDockInfoSplitter, ControlName);
  if Index = 0 then
    Exit;
  Result := FindDockForm(Copy(ControlName, 1, Index - 1));
  if Result <> nil then
  begin
    DockServer := FindDockServer(Result);
    if DockServer <> nil then
      with DockServer do
      begin
        if Pos('TopDockPanel', ControlName) > Index then
          Result := TopDockPanel
        else
        if Pos('LeftDockPanel', ControlName) > Index then
          Result := LeftDockPanel
        else
        if Pos('BottomDockPanel', ControlName) > Index then
          Result := BottomDockPanel
        else
        if Pos('RightDockPanel', ControlName) > Index then
          Result := RightDockPanel;
        if (Result <> nil) and (Pos('PopupPanel', ControlName) > 20) then
          Result := TJvDockVSNETPanel(Result).VSChannel.VSPopupPanel;
      end;
  end;
end;

function FindDockHost(ControlName: string): TWinControl;
begin
  Result := FindDockForm(ControlName);
  if Result = nil then
    Result := FindDockPanel(ControlName);
end;

//=== TJvDockInfoZone ========================================================

function TJvDockInfoZone.GetChildControlCount: Integer;
var
  Zone: TJvDockBaseZone;
begin
  Result := 0;
  if ChildZone <> nil then
  begin
    Inc(Result);
    Zone := ChildZone;
    while Zone.NextSibling <> nil do
    begin
      Zone := Zone.NextSibling;
      if TJvDockInfoZone(Zone).DockControl <> nil then
        Inc(Result);
    end;
  end;
end;

procedure TJvDockInfoZone.SetDockInfoFromControlToNode(Control: TControl);
begin
  DockRect := Control.BoundsRect;
  UnDockWidth := Control.UndockWidth;
  UnDockHeight := Control.UndockHeight;
  if Control is TJvDockVSPopupPanel then
    Control.Visible := False
  else
    Visible := Control.Visible;

  if Control is TForm then
  begin
    BorderStyle := TForm(Control).BorderStyle;
    FormStyle := TForm(Control).FormStyle;
    WindowState := TForm(Control).WindowState;
    LRDockWidth := Control.LRDockWidth;
    TBDockHeight := Control.TBDockHeight;
  end;
end;

procedure TJvDockInfoZone.SetDockInfoFromDockControlToNode(DockControl: TJvDockBaseControl);

  function GetLastDockSiteName(AControl: TControl): string;
  begin
    Result := RsDockCannotFindWindow;
    if AControl <> nil then
    begin
      if AControl.Parent is TJvDockableForm then
        Result := AControl.Parent.Name
      else
      if AControl is TJvDockPanel then
        Result := AControl.Parent.Name + RsDockJvDockInfoSplitter + AControl.Name;
    end;
  end;

begin
  CanDocked := DockControl.EnableDock;
  EachOtherDocked := DockControl.EachOtherDock;
  LeftDocked := DockControl.LeftDock;
  TopDocked := DockControl.TopDock;
  RightDocked := DockControl.RightDock;
  BottomDocked := DockControl.BottomDock;
  if DockControl is TJvDockClient then
  begin
    VSPaneWidth := TJvDockClient(DockControl).VSPaneWidth;
    UnDockLeft := TJvDockClient(DockControl).UnDockLeft;
    UnDockTop := TJvDockClient(DockControl).UnDockTop;
    LastDockSiteName := GetLastDockSiteName(TJvDockClient(DockControl).LastDockSite);
  end
  else
    VSPaneWidth := 0;
end;

procedure TJvDockInfoZone.SetDockInfoFromNodeToControl(Control: TControl);
var
  lbDockServer: TJvDockServer;

  procedure SetPopupPanelSize(PopupPanel: TJvDockVSPopupPanel);
  begin
  end;

  procedure SetDockSiteSize(DockSite: TJvDockPanel);
  begin
    if DockSite.Align in [alTop, alBottom] then
      DockSite.JvDockManager.DockSiteSize := DockRect.Bottom - DockRect.Top
    else
      DockSite.JvDockManager.DockSiteSize := DockRect.Right - DockRect.Left;
  end;

begin
  if (ParentName = '') or ((Control is TJvDockPanel) and
    (TJvDockPanel(Control).VisibleDockClientCount > 0)) then
  begin
    TWinControl(Control).DisableAlign;
    try
      if Control is TForm then
      begin
        TForm(Control).BorderStyle := BorderStyle;
        TForm(Control).FormStyle := FormStyle;
        if WindowState = wsNormal then
          Control.BoundsRect := DockRect;
        TForm(Control).WindowState := WindowState;
      end
      else
      begin
        if Control is TJvDockVSPopupPanel then
          SetPopupPanelSize(Control as TJvDockVSPopupPanel)
        else
          SetDockSiteSize(Control as TJvDockPanel);
      end;
      lbDockServer := FindDockServer(Control);
      if lbDockServer <> nil then
      begin
        lbDockServer.GetClientAlignControl(alTop);
        lbDockServer.GetClientAlignControl(alBottom);
        lbDockServer.GetClientAlignControl(alLeft);
        lbDockServer.GetClientAlignControl(alRight);
      end;
    finally
      TWinControl(Control).EnableAlign;
    end;
  end;
  Control.Visible := Visible;
  Control.LRDockWidth := LRDockWidth;
  Control.TBDockHeight := TBDockHeight;
  Control.UndockHeight := UndockHeight;
  Control.UndockWidth := UndockWidth;
end;

procedure TJvDockInfoZone.SetDockInfoFromNodeToDockControl(DockControl: TJvDockBaseControl);

  function GetLastDockSite(AName: string): TWinControl;
  begin
    Result := FindDockPanel(AName);
    if Result = nil then
    begin
      Result := FindDockForm(AName);
      if Result is TJvDockableForm then
        Result := TJvDockableForm(Result).DockableControl;
    end;
  end;

begin
  if DockControl is TJvDockClient then
  begin
    TJvDockClient(DockControl).UnDockLeft := UnDockLeft;
    TJvDockClient(DockControl).UnDockTop := UnDockTop;
    TJvDockClient(DockControl).LastDockSite := GetLastDockSite(LastDockSiteName);
    if Visible then
    begin
      TJvDockClient(DockControl).ParentVisible := False;
      TJvDockClient(DockControl).MakeShowEvent;
    end
    else
      TJvDockClient(DockControl).MakeHideEvent;
    TJvDockClient(DockControl).VSPaneWidth := VSPaneWidth;
  end;
  DockControl.EnableDock := CanDocked;
  DockControl.LeftDock := LeftDocked;
  DockControl.TopDock := TopDocked;
  DockControl.BottomDock := BottomDocked;
  DockControl.RightDock := RightDocked;
end;

//=== TJvDockInfoTree ========================================================

constructor TJvDockInfoTree.Create(TreeZone: TJvDockTreeZoneClass);
begin
  inherited Create(TreeZone);
  {$IFNDEF USEJVCL}
  FDockInfoIni := nil;
  FDockInfoReg := nil;
  {$ENDIF USEJVCL}
  FJvDockInfoStyle := isNone;
  FDataStream := TMemoryStream.Create;
end;

destructor TJvDockInfoTree.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FDataStream);
end;

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromApp(Control: TControl);
var
  I: TAlign;
  J:integer;
  TreeZone: TJvDockInfoZone;
  DockBaseControl: TJvDockBaseControl;
  TmpDockPanel: TJvDockPanel;
begin
  TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));
  with TreeZone do
  begin
    ParentName := TJvDockInfoZone(CurrTreeZone).DockFormName;
    SetDockInfoFromControlToNode(Control);
    if Control is TJvDockPanel then
      DockFormName := TJvDockInfoZone(CurrTreeZone).DockFormName +
        RsDockJvDockInfoSplitter + Control.Name
    else
      DockFormName := Control.Name;
    FDataStream.Clear;
    if Control is TJvDockTabHostForm then
      TJvDockTabHostForm(Control).PageControl.SaveToStream(FDataStream)
    else
    if Control is TJvDockConjoinHostForm then
      TJvDockConjoinHostForm(Control).Panel.DockManager.SaveToStream(FDataStream)
    else
    if Control is TJvDockPanel then
      TJvDockPanel(Control).DockManager.SaveToStream(FDataStream);
    DockClientData := JvDockStreamDataToString(FDataStream);
    DockBaseControl := FindDockBaseControl(Control);
    if DockBaseControl <> nil then
    begin
      SetDockInfoFromDockControlToNode(DockBaseControl);
      if Control is TJvDockTabHostForm then
        DockFormStyle := dsTab
      else
      if Control is TJvDockConjoinHostForm then
        DockFormStyle := dsConjoin
      else
        DockFormStyle := dsNormal;
      if DockBaseControl is TJvDockClient then
      begin
        if Control is TJvDockableForm then
          with TJvDockableForm(Control).DockableControl do
            for J := 0 to DockClientCount - 1 do
            begin
              CurrTreeZone := TreeZone;
              CreateZoneAndAddInfoFromApp(DockClients[J]);
              CurrTreeZone := TreeZone.GetParentZone;
            end;
      end
      else
      begin
        for I := alTop to alRight do
        begin
          CurrTreeZone := TreeZone;
          TmpDockPanel := TJvDockServer(DockBaseControl).DockPanelWithAlign[I];
          CreateZoneAndAddInfoFromApp(TmpDockPanel);
          if TmpDockPanel is TJvDockVSNETPanel then
            CreateZoneAndAddInfoFromApp(TJvDockVSNETPanel(TmpDockPanel).VSChannel.VSPopupPanel);
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;

    if Control is TJvDockPanel then
    begin
      DockFormStyle := dsDockPanel;
      if Control is TJvDockVSPopupPanel then
        with TJvDockVSPopupPanel(Control) do
          for J := 0 to DockClientCount - 1 do
          begin
            CurrTreeZone := TreeZone;
            CreateZoneAndAddInfoFromApp(TWinControl(DockClients[J]));
            CurrTreeZone := TreeZone.GetParentZone;
          end
      else
        with TJvDockPanel(Control) do
          for J := 0 to DockClientCount - 1 do
          begin
            CurrTreeZone := TreeZone;
            CreateZoneAndAddInfoFromApp(TWinControl(DockClients[J]));
            CurrTreeZone := TreeZone.GetParentZone;
          end;
    end;
  end;
end;

{$IFDEF USEJVCL}

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromAppStorage;
var
  FormList: TStringList;
  cp, cp1: PChar;
  S, APath: string;
  I: Integer;

  procedure CreateZoneAndAddInfo(Index: Integer);
  var
    I: Integer;
    TreeZone: TJvDockInfoZone;
    APath, OldPath: string;
  begin
    APath := FAppStorage.ConcatPaths([AppStoragePath,'Forms', FormList[Index]]);
    if FAppStorage.PathExists(APath) then
    begin
      TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));
      with TreeZone, FAppStorage do
      begin
        try
          OldPath := Path;
          Path := APath;
          DockFormName := FormList[Index];
          ParentName := ReadString('ParentName');
          DockRect := Rect(ReadInteger('DockLeft'), ReadInteger('DockTop'),
                           ReadInteger('DockRight'), ReadInteger('DockBottom'));
          LRDockWidth := ReadInteger('LRDockWidth');
          LastDockSiteName := ReadString('LastDockSiteName');
          UnDockLeft := ReadInteger('UnDockLeft');
          UnDockTop := ReadInteger('UnDockTop');
          TBDockHeight := ReadInteger('TBDockHeight');
          UnDockWidth := ReadInteger('UnDockWidth');
          UnDockHeight := ReadInteger('UnDockHeight');
          VSPaneWidth := ReadInteger('VSPaneWidth');
          Visible := ReadBoolean('Visible');
          BorderStyle := TBorderStyle(ReadInteger('BorderStyle'));
          FormStyle := TFormStyle(ReadInteger('FormStyle'));
          WindowState := TWindowState(ReadInteger('WindowState'));
          DockFormStyle := TJvDockFormStyle(ReadInteger('DockFormStyle'));
          CanDocked := ReadBoolean('CanDocked');
          EachOtherDocked := ReadBoolean('EachOtherDocked');
          LeftDocked := ReadBoolean('LeftDocked');
          TopDocked := ReadBoolean('TopDocked');
          RightDocked := ReadBoolean('RightDocked');
          BottomDocked := ReadBoolean('BottomDocked');
          DockClientData := ReadString('DockClientData');
        finally
          FAppStorage.Path := OldPath;
        end;
      end;
      for I := Index - 1 downto 0 do
      begin
        if FAppStorage.ReadString(FAppStorage.ConcatPaths([AppStoragePath, 'Forms', FormList[I], 'ParentName'])) = FormList[Index] then
        begin
          CurrTreeZone := TreeZone;
          CreateZoneAndAddInfo(I);
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;
  end;

begin
  FormList := TStringList.Create;
  FJvDockInfoStyle := isJVCLReadInfo;
  try
    APath := FAppStorage.ConcatPaths([AppStoragePath, 'Forms', 'FormNames']);
    if FAppStorage.ValueStored(APath) then
    begin
      S := FAppStorage.ReadString(APath);
      cp := PChar(S);
      cp1 := StrPos(cp, ';');
      while cp1 <> nil do
      begin
        cp1^ := #0;
        FormList.Add(string(cp));
        cp := cp1 + 1;
        cp1 := StrPos(cp, ';');
      end;
      for I := FormList.Count - 1 downto 0 do
        if FAppStorage.ReadString(FAppStorage.ConcatPaths([AppStoragePath, 'Forms', FormList[I], 'ParentName'])) = '' then
          CreateZoneAndAddInfo(I);
    end;
  finally
    FormList.Free;
    FJvDockInfoStyle := isNone;
  end;
end;

{$ENDIF USEJVCL}

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromIni;
var
  I: Integer;
  Sections: TStringList;
  TempDockInfoZoneArray: array of TJvDockInfoZone;

  procedure CreateTempDockInfoZoneArray;
  var
    I: Integer;
  begin
    SetLength(TempDockInfoZoneArray, SizeOf(TJvDockInfoZone) * Sections.Count);
    for I := 0 to Sections.Count - 1 do
    begin
      TempDockInfoZoneArray[I] := TJvDockInfoZone.Create(nil);
      with TempDockInfoZoneArray[I], DockInfoIni do
      begin
        DockFormName := Sections[I];
        ParentName := ReadString(DockFormName, 'ParentName', 'ERROR');
        DockRect := Rect(ReadInteger(DockFormName, 'DockLeft', 0),
          ReadInteger(DockFormName, 'DockTop', 0),
          ReadInteger(DockFormName, 'DockRight', 100),
          ReadInteger(DockFormName, 'DockBottom', 100));
        LastDockSiteName := ReadString(DockFormName, 'LastDockSiteName', 'ERROR');
        UnDockLeft := ReadInteger(DockFormName, 'UnDockLeft', 100);
        UnDockTop := ReadInteger(DockFormName, 'UnDockTop', 100);
        LRDockWidth := ReadInteger(DockFormName, 'LRDockWidth', 100);
        TBDockHeight := ReadInteger(DockFormName, 'TBDockHeight', 100);
        UnDockWidth := ReadInteger(DockFormName, 'UndockWidth', 100);
        UnDockHeight := ReadInteger(DockFormName, 'UndockHeight', 100);
        VSPaneWidth := ReadInteger(DockFormName, 'VSPaneWidth', 100);
        Visible := ReadBool(DockFormName, 'Visible', True);
        BorderStyle := TBorderStyle(ReadInteger(DockFormName, 'BorderStyle', 0));
        FormStyle := TFormStyle(ReadInteger(DockFormName, 'FormStyle', 0));
        WindowState := TWindowState(ReadInteger(DockFormName, 'WindowState', 0));
        DockFormStyle := TJvDockFormStyle(ReadInteger(DockFormName, 'DockFormStyle', 0));
        CanDocked := ReadBool(DockFormName, 'CanDocked', True);
        EachOtherDocked := ReadBool(DockFormName, 'EachOtherDocked', True);
        LeftDocked := ReadBool(DockFormName, 'LeftDocked', LeftDocked);
        TopDocked := ReadBool(DockFormName, 'TopDocked', True);
        RightDocked := ReadBool(DockFormName, 'RightDocked', True);
        BottomDocked := ReadBool(DockFormName, 'BottomDocked', True);
        DockClientData := ReadString(DockFormName, 'DockClientData', '');
      end;
    end;
  end;

  procedure DestroyTempDockInfoZoneArray;
  var
    I: Integer;
  begin
    for I := Sections.Count - 1 downto 0 do
      TempDockInfoZoneArray[I].Free;
  end;

  procedure CreateZoneAndAddInfo(Index: Integer);
  var
    I: Integer;
    TreeZone: TJvDockInfoZone;
  begin
    TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));

    TreeZone.DockFormName := TempDockInfoZoneArray[Index].DockFormName;
    TreeZone.ParentName := TempDockInfoZoneArray[Index].ParentName;
    TreeZone.DockRect := TempDockInfoZoneArray[Index].DockRect;
    TreeZone.LastDockSiteName := TempDockInfoZoneArray[Index].LastDockSiteName;
    TreeZone.UnDockLeft := TempDockInfoZoneArray[Index].UnDockLeft;
    TreeZone.UnDockTop := TempDockInfoZoneArray[Index].UnDockTop;
    TreeZone.LRDockWidth := TempDockInfoZoneArray[Index].LRDockWidth;
    TreeZone.TBDockHeight := TempDockInfoZoneArray[Index].TBDockHeight;
    TreeZone.UnDockWidth := TempDockInfoZoneArray[Index].UnDockWidth;
    TreeZone.UnDockHeight := TempDockInfoZoneArray[Index].UnDockHeight;
    TreeZone.VSPaneWidth := TempDockInfoZoneArray[Index].VSPaneWidth;
    TreeZone.Visible := TempDockInfoZoneArray[Index].Visible;
    TreeZone.BorderStyle := TempDockInfoZoneArray[Index].BorderStyle;
    TreeZone.FormStyle := TempDockInfoZoneArray[Index].FormStyle;
    TreeZone.WindowState := TempDockInfoZoneArray[Index].WindowState;
    TreeZone.DockFormStyle := TempDockInfoZoneArray[Index].DockFormStyle;
    TreeZone.CanDocked := TempDockInfoZoneArray[Index].CanDocked;
    TreeZone.EachOtherDocked := TempDockInfoZoneArray[Index].EachOtherDocked;
    TreeZone.LeftDocked := TempDockInfoZoneArray[Index].LeftDocked;
    TreeZone.TopDocked := TempDockInfoZoneArray[Index].TopDocked;
    TreeZone.RightDocked := TempDockInfoZoneArray[Index].RightDocked;
    TreeZone.BottomDocked := TempDockInfoZoneArray[Index].BottomDocked;
    TreeZone.DockClientData := TempDockInfoZoneArray[Index].DockClientData;

    for I := Index - 1 downto 0 do
      if TempDockInfoZoneArray[I].ParentName = Sections[Index] then
      begin
        CurrTreeZone := TreeZone;
        CreateZoneAndAddInfo(I);
        CurrTreeZone := TreeZone.GetParentZone;
      end;
  end;

begin
  Sections := TStringList.Create;
  try
    DockInfoIni.ReadSections(Sections);
    CreateTempDockInfoZoneArray;
    for I := Sections.Count - 1 downto 0 do
      if TempDockInfoZoneArray[I].ParentName = '' then
        CreateZoneAndAddInfo(I);
    FJvDockInfoStyle := isNone;
  finally
    DestroyTempDockInfoZoneArray;
    Sections.Free;
  end;
end;

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromReg;
var
  FormList: TStringList;
  cp, cp1: PChar;
  I: Integer;
  S: string;

  procedure CreateZoneAndAddInfo(Index: Integer);
  var
    I: Integer;
    TreeZone: TJvDockInfoZone;
  begin
    DockInfoReg.OpenKey(FRegName, False);
    if DockInfoReg.KeyExists(FormList[Index]) then
    begin
      DockInfoReg.OpenKey(FRegName + '\' + FormList[Index], False);
      TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));
      with TreeZone, DockInfoReg do
      begin
        DockFormName := FormList[Index];
        ParentName := ReadString('ParentName');
        DockRect := Rect(ReadInteger('DockLeft'), ReadInteger('DockTop'),
          ReadInteger('DockRight'), ReadInteger('DockBottom'));
        LRDockWidth := ReadInteger('LRDockWidth');
        LastDockSiteName := ReadString('LastDockSiteName');
        UnDockLeft := ReadInteger('UnDockLeft');
        UnDockTop := ReadInteger('UnDockTop');
        TBDockHeight := ReadInteger('TBDockHeight');
        UnDockWidth := ReadInteger('UnDockWidth');
        UnDockHeight := ReadInteger('UnDockHeight');
        VSPaneWidth := ReadInteger('VSPaneWidth');
        Visible := ReadBool('Visible');
        BorderStyle := TBorderStyle(ReadInteger('BorderStyle'));
        FormStyle := TFormStyle(ReadInteger('FormStyle'));
        WindowState := TWindowState(ReadInteger('WindowState'));
        DockFormStyle := TJvDockFormStyle(ReadInteger('DockFormStyle'));
        CanDocked := ReadBool('CanDocked');
        EachOtherDocked := ReadBool('EachOtherDocked');
        LeftDocked := ReadBool('LeftDocked');
        TopDocked := ReadBool('TopDocked');
        RightDocked := ReadBool('RightDocked');
        BottomDocked := ReadBool('BottomDocked');
        DockClientData := ReadString('DockClientData');
      end;
      for I := Index - 1 downto 0 do
      begin
        DockInfoReg.OpenKey(FRegName + '\' + FormList[I], False);
        if DockInfoReg.ReadString('ParentName') = FormList[Index] then
        begin
          CurrTreeZone := TreeZone;
          CreateZoneAndAddInfo(I);
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;
  end;

begin
  FormList := TStringList.Create;
  try
    if DockInfoReg.OpenKey(FRegName, False) then
    begin
      S := DockInfoReg.ReadString('FormNames');
      cp := PChar(S);
      cp1 := StrPos(cp, '\');
      while cp1 <> nil do
      begin
        cp1^ := #0;
        FormList.Add(string(cp));
        cp := cp1 + 1;
        cp1 := StrPos(cp, '\');
      end;
      FJvDockInfoStyle := isReadFileInfo;
      for I := FormList.Count - 1 downto 0 do
      begin
        DockInfoReg.OpenKey(FRegName + '\' + FormList[I], False);
        if DockInfoReg.ReadString('ParentName') = '' then
          CreateZoneAndAddInfo(I);
      end;
      FJvDockInfoStyle := isNone;
    end;
  finally
    DockInfoReg.CloseKey;
    FormList.Free;
  end;
end;

{$IFDEF USEJVCL}

procedure TJvDockInfoTree.ReadInfoFromAppStorage;
begin
  CreateZoneAndAddInfoFromAppStorage;

  DoFloatAllForm;

  // (rom) this is disputable
  Application.ProcessMessages;

  try
    FJvDockInfoStyle := isJVCLReadInfo;
    MiddleScanTree(TopTreeZone);
  finally
    FJvDockInfoStyle := isNone;
  end;
end;

{$ENDIF USEJVCL}

procedure TJvDockInfoTree.ReadInfoFromIni;
begin
  CreateZoneAndAddInfoFromIni;

  DoFloatAllForm;

  // (rom) this is disputable
  Application.ProcessMessages;

  {$IFDEF USEJVCL}
  FJvDockInfoStyle := isJVCLReadInfo;
  {$ELSE}
  FJvDockInfoStyle := isReadFileInfo;
  {$ENDIF USEJVCL}
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

procedure TJvDockInfoTree.ReadInfoFromReg(RegName: string);
begin
  FRegName := RegName;
  CreateZoneAndAddInfoFromReg;

  DoFloatAllForm;

  // (rom) this is disputable
  Application.ProcessMessages;

  FJvDockInfoStyle := isReadRegInfo;
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

{$IFDEF USEJVCL}

procedure TJvDockInfoTree.ScanTreeZone(TreeZone: TJvDockBaseZone);
var
  I: Integer;
  APath, OldPath: string;
begin
  if (FJvDockInfoStyle = isJVCLReadInfo) then
  begin
    for I := 0 to TreeZone.GetChildCount - 1 do
    begin
      with TJvDockInfoZone(TreeZone.GetChildZone(I)) do
        DockControl := FindDockForm(DockFormName);
    end;
    SetDockControlInfo(TJvDockInfoZone(TreeZone));
  end
  else if FJvDockInfoStyle = isJVCLWriteInfo then
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), FAppStorage do
      begin
        APath := ConcatPaths([AppStoragePath, 'Forms', 'FormNames']);
        WriteString(APath, ReadString(APath) + DockFormName + ';');
        try
          OldPath := Path;
          Path := ConcatPaths([OldPath, AppStoragePath, 'Forms', DockFormName]);
          WriteString('ParentName', ParentName);
          WriteInteger('DockLeft', DockRect.Left);
          WriteInteger('DockTop', DockRect.Top);
          WriteInteger('DockRight', DockRect.Right);
          WriteInteger('DockBottom', DockRect.Bottom);
          WriteString('LastDockSiteName', LastDockSiteName);
          WriteInteger('UnDockLeft', UnDockLeft);
          WriteInteger('UnDockTop', UnDockTop);
          WriteInteger('LRDockWidth', LRDockWidth);
          WriteInteger('TBDockHeight', TBDockHeight);
          WriteInteger('UnDockWidth', UnDockWidth);
          WriteInteger('UnDockHeight', UnDockHeight);
          WriteInteger('VSPaneWidth', VSPaneWidth);
          WriteBoolean('Visible', Visible);
          WriteInteger('BorderStyle', Integer(BorderStyle));
          WriteInteger('FormStyle', Integer(FormStyle));
          WriteInteger('WindowState', Integer(WindowState));
          WriteInteger('DockFormStyle', Integer(DockFormStyle));
          WriteBoolean('CanDocked', CanDocked);
          WriteBoolean('EachOtherDocked', EachOtherDocked);
          WriteBoolean('LeftDocked', LeftDocked);
          WriteBoolean('TopDocked', TopDocked);
          WriteBoolean('RightDocked', RightDocked);
          WriteBoolean('BottomDocked', BottomDocked);
          WriteString('DockClientData', DockClientData);
        finally
          FAppStorage.Path := OldPath;
        end;
      end;
  end;
  inherited ScanTreeZone(TreeZone);
end;

{$ELSE}

procedure TJvDockInfoTree.ScanTreeZone(TreeZone: TJvDockBaseZone);
var
  I: Integer;
begin
//  FJvDockInfoStyle := isReadFileInfo;
  if (FJvDockInfoStyle = isReadFileInfo) or (FJvDockInfoStyle = isReadRegInfo) then
  begin
    for I := 0 to TreeZone.GetChildCount - 1 do
    begin
      with TJvDockInfoZone(TreeZone.GetChildZone(I)) do
        DockControl := FindDockForm(DockFormName);
    end;
    SetDockControlInfo(TJvDockInfoZone(TreeZone));
  end
  else if FJvDockInfoStyle = isWriteFileInfo then
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), DockInfoIni do
      begin
        WriteString(DockFormName, 'ParentName', ParentName);
        WriteInteger(DockFormName, 'DockLeft', DockRect.Left);
        WriteInteger(DockFormName, 'DockTop', DockRect.Top);
        WriteInteger(DockFormName, 'DockRight', DockRect.Right);
        WriteInteger(DockFormName, 'DockBottom', DockRect.Bottom);
        WriteString(DockFormName, 'LastDockSiteName', LastDockSiteName);
        WriteInteger(DockFormName, 'UnDockLeft', UnDockLeft);
        WriteInteger(DockFormName, 'UnDockTop', UnDockTop);
        WriteInteger(DockFormName, 'LRDockWidth', LRDockWidth);
        WriteInteger(DockFormName, 'TBDockHeight', TBDockHeight);
        WriteInteger(DockFormName, 'UnDockWidth', UnDockWidth);
        WriteInteger(DockFormName, 'UnDockHeight', UnDockHeight);
        WriteInteger(DockFormName, 'VSPaneWidth', VSPaneWidth);
        WriteBool(DockFormName, 'Visible', Visible);
        WriteInteger(DockFormName, 'BorderStyle', Integer(BorderStyle));
        WriteInteger(DockFormName, 'WindowState', Integer(WindowState));
        WriteInteger(DockFormName, 'FormStyle', Integer(FormStyle));
        WriteInteger(DockFormName, 'DockFormStyle', Integer(DockFormStyle));
        WriteBool(DockFormName, 'CanDocked', CanDocked);
        WriteBool(DockFormName, 'EachOtherDocked', EachOtherDocked);
        WriteBool(DockFormName, 'LeftDocked', LeftDocked);
        WriteBool(DockFormName, 'TopDocked', TopDocked);
        WriteBool(DockFormName, 'RightDocked', RightDocked);
        WriteBool(DockFormName, 'BottomDocked', BottomDocked);
        WriteString(DockFormName, 'DockClientData', DockClientData);
      end;
  end
  else
  if FJvDockInfoStyle = isWriteRegInfo then
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), DockInfoReg do
      begin
        OpenKey(FRegName, True);
        WriteString('FormNames', ReadString('FormNames') + DockFormName + '\');
        OpenKey(FRegName + '\' + DockFormName, True);
        WriteString('ParentName', ParentName);
        WriteInteger('DockLeft', DockRect.Left);
        WriteInteger('DockTop', DockRect.Top);
        WriteInteger('DockRight', DockRect.Right);
        WriteInteger('DockBottom', DockRect.Bottom);
        WriteString('LastDockSiteName', LastDockSiteName);
        WriteInteger('UnDockLeft', UnDockLeft);
        WriteInteger('UnDockTop', UnDockTop);
        WriteInteger('LRDockWidth', LRDockWidth);
        WriteInteger('TBDockHeight', TBDockHeight);
        WriteInteger('UnDockWidth', UnDockWidth);
        WriteInteger('UnDockHeight', UnDockHeight);
        WriteInteger('VSPaneWidth', VSPaneWidth);
        WriteBool('Visible', Visible);
        WriteInteger('BorderStyle', Integer(BorderStyle));
        WriteInteger('FormStyle', Integer(FormStyle));
        WriteInteger('WindowState', Integer(WindowState));
        WriteInteger('DockFormStyle', Integer(DockFormStyle));
        WriteBool('CanDocked', CanDocked);
        WriteBool('EachOtherDocked', EachOtherDocked);
        WriteBool('LeftDocked', LeftDocked);
        WriteBool('TopDocked', TopDocked);
        WriteBool('RightDocked', RightDocked);
        WriteBool('BottomDocked', BottomDocked);
        WriteString('DockClientData', DockClientData);
        CloseKey;
      end;
  end;
  inherited ScanTreeZone(TreeZone);
end;

{$ENDIF USEJVCL}

function TJvDockInfoTree.FindDockForm(FormName: string): TCustomForm;
begin
  if Pos(RsDockJvDockInfoSplitter, FormName) > 0 then
    Result := nil
  else
    Result := JvDockFindDockFormWithName(FormName);
end;

function TJvDockInfoTree.CreateHostControl(ATreeZone: TJvDockInfoZone): TWinControl;
var
  Form: TForm;
  DockClient: TJvDockClient;
begin
  Result := nil;
  case ATreeZone.DockFormStyle of
    dsConjoin:
      begin
        Form := TJvDockConjoinHostForm.Create(Application);
        DockClient := FindDockClient(TJvDockInfoZone(ATreeZone.ChildZone).DockControl);
        Result := DockClient.CreateConjoinPanelClass(Form).Parent;
      end;
    dsTab:
      begin
        Form := TJvDockTabHostForm.Create(Application);
        DockClient := FindDockClient(TJvDockInfoZone(ATreeZone.ChildZone).DockControl);
        Result := DockClient.CreateTabDockClass(Form).Parent;
      end;
  end;
  if Result <> nil then
    Result.Name := ATreeZone.DockFormName;
end;

procedure TJvDockInfoTree.SetDockControlInfo(ATreeZone: TJvDockInfoZone);
var
  DockBaseControl: TJvDockBaseControl;
  Host: TWinControl;
begin
  with ATreeZone do
  begin
    if DockFormName = '' then
      Exit;
    Host := FindDockHost(DockFormName);
    if (Host = nil) and (ATreeZone.GetChildControlCount > 1) then
      Host := CreateHostControl(ATreeZone);
    if (Host <> nil) and (DockClientData <> '') and (FDataStream <> nil) then
    begin
      FDataStream.Clear;

      JvDockStringToStreamData(FDataStream, DockClientData);

      FDataStream.Position := 0;
      if Host is TJvDockTabHostForm then
      begin
        with TJvDockTabHostForm(Host).PageControl do
        begin
          DisableAlign;
          try
            LoadFromStream(FDataStream);
          finally
            EnableAlign;
          end;
        end;
      end
      else
      if Host is TJvDockConjoinHostForm then
      begin
        with TJvDockConjoinHostForm(Host).Panel do
        begin
          DisableAlign;
          try
            DockManager.LoadFromStream(FDataStream);
          finally
            EnableAlign;
          end;
        end;
      end
      else
      if Host is TJvDockPanel then
      begin
        with TJvDockPanel(Host) do
        begin
          DisableAlign;
          try
            DockManager.LoadFromStream(FDataStream);
          finally
            EnableAlign;
          end;
        end;
      end;
    end;
    if Host <> nil then
    begin
      SetDockInfoFromNodeToControl(Host);
      DockBaseControl := FindDockBaseControl(Host);
      if DockBaseControl <> nil then
        SetDockInfoFromNodeToDockControl(DockBaseControl);
    end;
  end;
end;

{$IFDEF USEJVCL}

procedure TJvDockInfoTree.WriteInfoToAppStorage;
begin
  AppStorage.DeleteSubTree(AppStoragePath);
  try
    FJvDockInfoStyle := isJVCLWriteInfo;
    MiddleScanTree(TopTreeZone);
  finally
    FJvDockInfoStyle := isNone;
  end;
end;

{$ENDIF USEJVCL}

procedure TJvDockInfoTree.WriteInfoToIni;
var
  Sections: TStringList;
  I: Integer;
begin
  Sections := TStringList.Create;
  try
    DockInfoIni.ReadSections(Sections);

    for I := 0 to Sections.Count - 1 do
      DockInfoIni.EraseSection(Sections[I]);
  finally
    Sections.Free;
  end;
  {$IFDEF USEJVCL}
  FJvDockInfoStyle := isJVCLWriteInfo;
  {$ELSE}
  FJvDockInfoStyle := isWriteFileInfo;
  {$ENDIF USEJVCL}
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

procedure TJvDockInfoTree.WriteInfoToReg(RegName: string);
begin
  try
    if DockInfoReg.OpenKey(RegName, False) then
      DockInfoReg.DeleteKey(RegName);

    DockInfoReg.CreateKey(RegName);
    DockInfoReg.CloseKey;
    FRegName := RegName;

    FJvDockInfoStyle := isWriteRegInfo;
    MiddleScanTree(TopTreeZone);
    FJvDockInfoStyle := isNone;
  finally
    DockInfoReg.CloseKey;
  end;
end;

{$IFDEF USEJVCL}
function TJvDockInfoTree.GetAppStoragePath: string;
begin
  Result := FAppStoragePath;
  if (Result = '') and (FAppStorage <> nil) then
    Result := FAppStorage.Path;
end;
{$ENDIF}

end.

