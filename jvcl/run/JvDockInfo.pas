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

  {$IFDEF USEJVCL}
  TJvDockInfoStyle =
    (isNone, isReadInfo, isWriteInfo);
  {$ELSE}
  TJvDockInfoStyle =
    (isNone, isReadFileInfo, isWriteFileInfo, isReadRegInfo, isWriteRegInfo);
  {$ENDIF USEJVCL}

  TJvDockInfoTree = class(TJvDockBaseTree)
  private
    {$IFDEF USEJVCL}
    FAppStorage: TJvCustomAppStorage;
    FAppStoragePath: string;
    {$ELSE}
    FDockInfoIni: TIniFile;
    FDockInfoReg: TRegistry;
    FRegName: string;
    {$ENDIF USEJVCL}
    FJvDockInfoStyle: TJvDockInfoStyle;
    FDataStream: TMemoryStream;
    function FindDockForm(FormName: string): TCustomForm;
    function CreateHostControl(ATreeZone: TJvDockInfoZone): TWinControl;
  protected
    procedure ScanTreeZone(TreeZone: TJvDockBaseZone); override;
    {$IFDEF USEJVCL}
    procedure CreateZoneAndAddInfoFromAppStorage; virtual;
    {$ELSE}
    procedure CreateZoneAndAddInfoFromIni; virtual;
    procedure CreateZoneAndAddInfoFromReg; virtual;
    {$ENDIF USEJVCL}
    procedure SetDockControlInfo(ATreeZone: TJvDockInfoZone); virtual;
  public
    constructor Create(TreeZone: TJvDockTreeZoneClass); override;
    destructor Destroy; override;
    procedure CreateZoneAndAddInfoFromApp(Control: TControl); virtual;

    {$IFDEF USEJVCL}
    procedure ReadInfoFromAppStorage;
    procedure WriteInfoToAppStorage;
    property AppStorage: TJvCustomAppStorage read FAppStorage write FAppStorage;
    property AppStoragePath: string read FAppStoragePath write FAppStoragePath;
    {$ELSE}
    procedure ReadInfoFromIni;
    procedure ReadInfoFromReg(RegName: string);
    procedure WriteInfoToIni;
    procedure WriteInfoToReg(RegName: string);
    property DockInfoIni: TIniFile read FDockInfoIni write FDockInfoIni;
    property DockInfoReg: TRegistry read FDockInfoReg write FDockInfoReg;
    {$ENDIF USEJVCL}
  end;

implementation

uses
  JvDockGlobals, JvDockVSNetStyle;

const
  cBorderStyle = 'BorderStyle';
  cBottomDocked = 'BottomDocked';
  cBottomDockPanel = 'BottomDockPanel';
  cCanDocked = 'CanDocked';
  cDockBottom = 'DockBottom';
  cDockClientData = 'DockClientData';
  cDockFormStyle = 'DockFormStyle';
  cDockLeft = 'DockLeft';
  cDockRight = 'DockRight';
  cDockTop = 'DockTop';
  cEachOtherDocked = 'EachOtherDocked';
  cERROR = 'ERROR';
  cFormNames = 'FormNames';
  cForms = 'Forms';
  cFormStyle = 'FormStyle';
  cLastDockSiteName = 'LastDockSiteName';
  cLeftDocked = 'LeftDocked';
  cLeftDockPanel = 'LeftDockPanel';
  cLRDockWidth = 'LRDockWidth';
  cParentName = 'ParentName';
  cPopupPanel = 'PopupPanel';
  cRightDocked = 'RightDocked';
  cRightDockPanel = 'RightDockPanel';
  cTBDockHeight = 'TBDockHeight';
  cTopDocked = 'TopDocked';
  cTopDockPanel = 'TopDockPanel';
  cUnDockHeight = 'UnDockHeight';
  cUnDockLeft = 'UnDockLeft';
  cUnDockTop = 'UnDockTop';
  cUnDockWidth = 'UnDockWidth';
  cVisible = 'Visible';
  cVSPaneWidth = 'VSPaneWidth';
  cWindowState = 'WindowState';

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
        if Pos(cTopDockPanel, ControlName) > Index then
          Result := TopDockPanel
        else
        if Pos(cLeftDockPanel, ControlName) > Index then
          Result := LeftDockPanel
        else
        if Pos(cBottomDockPanel, ControlName) > Index then
          Result := BottomDockPanel
        else
        if Pos(cRightDockPanel, ControlName) > Index then
          Result := RightDockPanel;
        if (Result <> nil) and (Pos(cPopupPanel, ControlName) > 20) then
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
      DockSite.lbDockManager.DockSiteSize := DockRect.Bottom - DockRect.Top
    else
      DockSite.lbDockManager.DockSiteSize := DockRect.Right - DockRect.Left;
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
  FDataStream.Free;
  inherited Destroy;
end;

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromApp(Control: TControl);
var
  I: Integer;
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
            for I := 0 to DockClientCount - 1 do
            begin
              CurrTreeZone := TreeZone;
              CreateZoneAndAddInfoFromApp(DockClients[I]);
              CurrTreeZone := TreeZone.GetParentZone;
            end;
      end
      else
      begin
        for I := 0 to 3 do
        begin
          CurrTreeZone := TreeZone;
          TmpDockPanel := TJvDockServer(DockBaseControl).DockPanel[I];
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
          for I := 0 to DockClientCount - 1 do
          begin
            CurrTreeZone := TreeZone;
            CreateZoneAndAddInfoFromApp(TWinControl(DockClients[I]));
            CurrTreeZone := TreeZone.GetParentZone;
          end
      else
        with TJvDockPanel(Control) do
          for I := 0 to DockClientCount - 1 do
          begin
            CurrTreeZone := TreeZone;
            CreateZoneAndAddInfoFromApp(TWinControl(DockClients[I]));
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
  S: string;
  I: Integer;

  procedure CreateZoneAndAddInfo(Index: Integer);
  var
    I: Integer;
    TreeZone: TJvDockInfoZone;
    OldPath: string;
  begin
    if FAppStorage.PathExists(FAppStorage.ConcatPaths([AppStoragePath, cForms, FormList[Index]])) then
    begin
      TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));
      with TreeZone, FAppStorage do
      begin
        try
          OldPath := Path;
          Path := AppStorage.ConcatPaths([OldPath,AppStoragePath, cForms, FormList[Index]]);
          DockFormName := FormList[Index];
          ParentName := ReadString(cParentName);
          DockRect := Rect(ReadInteger(cDockLeft), ReadInteger(cDockTop),
                           ReadInteger(cDockRight), ReadInteger(cDockBottom));
          LRDockWidth := ReadInteger(cLRDockWidth);
          LastDockSiteName := ReadString(cLastDockSiteName);
          UnDockLeft := ReadInteger(cUnDockLeft);
          UnDockTop := ReadInteger(cUnDockTop);
          TBDockHeight := ReadInteger(cTBDockHeight);
          UnDockWidth := ReadInteger(cUnDockWidth);
          UnDockHeight := ReadInteger(cUnDockHeight);
          VSPaneWidth := ReadInteger(cVSPaneWidth);
          Visible := ReadBoolean(cVisible);
          BorderStyle := TBorderStyle(ReadInteger(cBorderStyle));
          FormStyle := TFormStyle(ReadInteger(cFormStyle));
          WindowState := TWindowState(ReadInteger(cWindowState));
          DockFormStyle := TJvDockFormStyle(ReadInteger(cDockFormStyle));
          CanDocked := ReadBoolean(cCanDocked);
          EachOtherDocked := ReadBoolean(cEachOtherDocked);
          LeftDocked := ReadBoolean(cLeftDocked);
          TopDocked := ReadBoolean(cTopDocked);
          RightDocked := ReadBoolean(cRightDocked);
          BottomDocked := ReadBoolean(cBottomDocked);
          DockClientData := ReadString(cDockClientData);
        finally
          FAppStorage.Path := OldPath;
        end;
      end;
      for I := Index - 1 downto 0 do
      begin
        if FAppStorage.ReadString(FAppStorage.ConcatPaths([AppStoragePath, cForms, FormList[I], cParentName])) = FormList[Index] then
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
    if FAppStorage.ValueStored(FAppStorage.ConcatPaths([AppStoragePath, cForms, cFormNames])) then
    begin
      S := FAppStorage.ReadString(FAppStorage.ConcatPaths([AppStoragePath, cForms, cFormNames]));
      cp := PChar(S);
      cp1 := StrPos(cp, ';');
      while cp1 <> nil do
      begin
        cp1^ := #0;
        FormList.Add(string(cp));
        cp := cp1 + 1;
        cp1 := StrPos(cp, ';');
      end;
      FJvDockInfoStyle := isReadInfo;
      for I := FormList.Count - 1 downto 0 do
        if FAppStorage.ReadString(FAppStorage.ConcatPaths([AppStoragePath, cForms, FormList[I], cParentName])) = '' then
          CreateZoneAndAddInfo(I);
      FJvDockInfoStyle := isNone;
    end;
  finally
    FormList.Free;
  end;
end;

{$ELSE}

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
        ParentName := ReadString(DockFormName, cParentName, cERROR);
        DockRect := Rect(ReadInteger(DockFormName, cDockLeft, 0),
          ReadInteger(DockFormName, cDockTop, 0),
          ReadInteger(DockFormName, cDockRight, 100),
          ReadInteger(DockFormName, cDockBottom, 100));
        LastDockSiteName := ReadString(DockFormName, cLastDockSiteName, cERROR);
        UnDockLeft := ReadInteger(DockFormName, cUnDockLeft, 100);
        UnDockTop := ReadInteger(DockFormName, cUnDockTop, 100);
        LRDockWidth := ReadInteger(DockFormName, cLRDockWidth, 100);
        TBDockHeight := ReadInteger(DockFormName, cTBDockHeight, 100);
        UnDockWidth := ReadInteger(DockFormName, cUndockWidth, 100);
        UnDockHeight := ReadInteger(DockFormName, cUndockHeight, 100);
        VSPaneWidth := ReadInteger(DockFormName, cVSPaneWidth, 100);
        Visible := ReadBool(DockFormName, cVisible, True);
        BorderStyle := TBorderStyle(ReadInteger(DockFormName, cBorderStyle, 0));
        FormStyle := TFormStyle(ReadInteger(DockFormName, cFormStyle, 0));
        WindowState := TWindowState(ReadInteger(DockFormName, cWindowState, 0));
        DockFormStyle := TJvDockFormStyle(ReadInteger(DockFormName, cDockFormStyle, 0));
        CanDocked := ReadBool(DockFormName, cCanDocked, True);
        EachOtherDocked := ReadBool(DockFormName, cEachOtherDocked, True);
        LeftDocked := ReadBool(DockFormName, cLeftDocked, LeftDocked);
        TopDocked := ReadBool(DockFormName, cTopDocked, True);
        RightDocked := ReadBool(DockFormName, cRightDocked, True);
        BottomDocked := ReadBool(DockFormName, cBottomDocked, True);
        DockClientData := ReadString(DockFormName, cDockClientData, '');
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
    FJvDockInfoStyle := isReadFileInfo;

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
        ParentName := ReadString(cParentName);
        DockRect := Rect(ReadInteger(cDockLeft), ReadInteger(cDockTop),
          ReadInteger(cDockRight), ReadInteger(cDockBottom));
        LRDockWidth := ReadInteger(cLRDockWidth);
        LastDockSiteName := ReadString(cLastDockSiteName);
        UnDockLeft := ReadInteger(cUnDockLeft);
        UnDockTop := ReadInteger(cUnDockTop);
        TBDockHeight := ReadInteger(cTBDockHeight);
        UnDockWidth := ReadInteger(cUnDockWidth);
        UnDockHeight := ReadInteger(cUnDockHeight);
        VSPaneWidth := ReadInteger(cVSPaneWidth);
        Visible := ReadBool(cVisible);
        BorderStyle := TBorderStyle(ReadInteger(cBorderStyle));
        FormStyle := TFormStyle(ReadInteger(cFormStyle));
        WindowState := TWindowState(ReadInteger(cWindowState));
        DockFormStyle := TJvDockFormStyle(ReadInteger(cDockFormStyle));
        CanDocked := ReadBool(cCanDocked);
        EachOtherDocked := ReadBool(cEachOtherDocked);
        LeftDocked := ReadBool(cLeftDocked);
        TopDocked := ReadBool(cTopDocked);
        RightDocked := ReadBool(cRightDocked);
        BottomDocked := ReadBool(cBottomDocked);
        DockClientData := ReadString(cDockClientData);
      end;
      for I := Index - 1 downto 0 do
      begin
        DockInfoReg.OpenKey(FRegName + '\' + FormList[I], False);
        if DockInfoReg.ReadString(cParentName) = FormList[Index] then
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
      S := DockInfoReg.ReadString(cFormNames);
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
        if DockInfoReg.ReadString(cParentName) = '' then
          CreateZoneAndAddInfo(I);
      end;
      FJvDockInfoStyle := isNone;
    end;
  finally
    DockInfoReg.CloseKey;
    FormList.Free;
  end;
end;

{$ENDIF USEJVCL}

{$IFDEF USEJVCL}

procedure TJvDockInfoTree.ReadInfoFromAppStorage;
begin
  CreateZoneAndAddInfoFromAppStorage;

  DoFloatAllForm;

  // (rom) this is disputable
  Application.ProcessMessages;

  try
    FJvDockInfoStyle := isReadInfo;
    MiddleScanTree(TopTreeZone);
  finally
    FJvDockInfoStyle := isNone;
  end;
end;

procedure TJvDockInfoTree.ScanTreeZone(TreeZone: TJvDockBaseZone);
var
  I: Integer;
  OldPath: string;
begin
  if (FJvDockInfoStyle = isReadInfo) then
  begin
    for I := 0 to TreeZone.GetChildCount - 1 do
    begin
      with TJvDockInfoZone(TreeZone.GetChildZone(I)) do
        DockControl := FindDockForm(DockFormName);
    end;
    SetDockControlInfo(TJvDockInfoZone(TreeZone));
  end
  else
  if FJvDockInfoStyle = isWriteInfo then
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), FAppStorage do
      begin
        WriteString(ConcatPaths([FAppStoragePath, cForms, cFormNames]), ReadString(ConcatPaths([FAppStoragePath, cForms, cFormNames])) + DockFormName + ';');
        try
          OldPath := Path;
          Path := ConcatPaths([OldPath, FAppStoragePath, cForms, DockFormName]);
          WriteString(cParentName, ParentName);
          WriteInteger(cDockLeft, DockRect.Left);
          WriteInteger(cDockTop, DockRect.Top);
          WriteInteger(cDockRight, DockRect.Right);
          WriteInteger(cDockBottom, DockRect.Bottom);
          WriteString(cLastDockSiteName, LastDockSiteName);
          WriteInteger(cUnDockLeft, UnDockLeft);
          WriteInteger(cUnDockTop, UnDockTop);
          WriteInteger(cLRDockWidth, LRDockWidth);
          WriteInteger(cTBDockHeight, TBDockHeight);
          WriteInteger(cUnDockWidth, UnDockWidth);
          WriteInteger(cUnDockHeight, UnDockHeight);
          WriteInteger(cVSPaneWidth, VSPaneWidth);
          WriteBoolean(cVisible, Visible);
          WriteInteger(cBorderStyle, Integer(BorderStyle));
          WriteInteger(cFormStyle, Integer(FormStyle));
          WriteInteger(cWindowState, Integer(WindowState));
          WriteInteger(cDockFormStyle, Integer(DockFormStyle));
          WriteBoolean(cCanDocked, CanDocked);
          WriteBoolean(cEachOtherDocked, EachOtherDocked);
          WriteBoolean(cLeftDocked, LeftDocked);
          WriteBoolean(cTopDocked, TopDocked);
          WriteBoolean(cRightDocked, RightDocked);
          WriteBoolean(cBottomDocked, BottomDocked);
          WriteString(cDockClientData, DockClientData);
        finally
          FAppStorage.Path := OldPath;
        end;
      end;
  end;
  inherited ScanTreeZone(TreeZone);
end;

{$ELSE}

procedure TJvDockInfoTree.ReadInfoFromIni;
begin
  CreateZoneAndAddInfoFromIni;

  DoFloatAllForm;

  // (rom) this is disputable
  Application.ProcessMessages;

  FJvDockInfoStyle := isReadFileInfo;
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

procedure TJvDockInfoTree.ScanTreeZone(TreeZone: TJvDockBaseZone);
var
  I: Integer;
begin
  if (FJvDockInfoStyle = isReadFileInfo) or (FJvDockInfoStyle = isReadRegInfo) then
  begin
    for I := 0 to TreeZone.GetChildCount - 1 do
    begin
      with TJvDockInfoZone(TreeZone.GetChildZone(I)) do
        DockControl := FindDockForm(DockFormName);
    end;
    SetDockControlInfo(TJvDockInfoZone(TreeZone));
  end
  else
  if FJvDockInfoStyle = isWriteFileInfo then
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), DockInfoIni do
      begin
        WriteString(DockFormName, cParentName, ParentName);
        WriteInteger(DockFormName, cDockLeft, DockRect.Left);
        WriteInteger(DockFormName, cDockTop, DockRect.Top);
        WriteInteger(DockFormName, cDockRight, DockRect.Right);
        WriteInteger(DockFormName, cDockBottom, DockRect.Bottom);
        WriteString(DockFormName, cLastDockSiteName, LastDockSiteName);
        WriteInteger(DockFormName, cUnDockLeft, UnDockLeft);
        WriteInteger(DockFormName, cUnDockTop, UnDockTop);
        WriteInteger(DockFormName, cLRDockWidth, LRDockWidth);
        WriteInteger(DockFormName, cTBDockHeight, TBDockHeight);
        WriteInteger(DockFormName, cUnDockWidth, UnDockWidth);
        WriteInteger(DockFormName, cUnDockHeight, UnDockHeight);
        WriteInteger(DockFormName, cVSPaneWidth, VSPaneWidth);
        WriteBool(DockFormName, cVisible, Visible);
        WriteInteger(DockFormName, cBorderStyle, Integer(BorderStyle));
        WriteInteger(DockFormName, cWindowState, Integer(WindowState));
        WriteInteger(DockFormName, cFormStyle, Integer(FormStyle));
        WriteInteger(DockFormName, cDockFormStyle, Integer(DockFormStyle));
        WriteBool(DockFormName, cCanDocked, CanDocked);
        WriteBool(DockFormName, cEachOtherDocked, EachOtherDocked);
        WriteBool(DockFormName, cLeftDocked, LeftDocked);
        WriteBool(DockFormName, cTopDocked, TopDocked);
        WriteBool(DockFormName, cRightDocked, RightDocked);
        WriteBool(DockFormName, cBottomDocked, BottomDocked);
        WriteString(DockFormName, cDockClientData, DockClientData);
      end;
  end
  else
  if FJvDockInfoStyle = isWriteRegInfo then
  begin
    if TreeZone <> TopTreeZone then
      with TJvDockInfoZone(TreeZone), DockInfoReg do
      begin
        OpenKey(FRegName, True);
        WriteString(cFormNames, ReadString(cFormNames) + DockFormName + '\');
        OpenKey(FRegName + '\' + DockFormName, True);
        WriteString(cParentName, ParentName);
        WriteInteger(cDockLeft, DockRect.Left);
        WriteInteger(cDockTop, DockRect.Top);
        WriteInteger(cDockRight, DockRect.Right);
        WriteInteger(cDockBottom, DockRect.Bottom);
        WriteString(cLastDockSiteName, LastDockSiteName);
        WriteInteger(cUnDockLeft, UnDockLeft);
        WriteInteger(cUnDockTop, UnDockTop);
        WriteInteger(cLRDockWidth, LRDockWidth);
        WriteInteger(cTBDockHeight, TBDockHeight);
        WriteInteger(cUnDockWidth, UnDockWidth);
        WriteInteger(cUnDockHeight, UnDockHeight);
        WriteInteger(cVSPaneWidth, VSPaneWidth);
        WriteBool(cVisible, Visible);
        WriteInteger(cBorderStyle, Integer(BorderStyle));
        WriteInteger(cFormStyle, Integer(FormStyle));
        WriteInteger(cWindowState, Integer(WindowState));
        WriteInteger(cDockFormStyle, Integer(DockFormStyle));
        WriteBool(cCanDocked, CanDocked);
        WriteBool(cEachOtherDocked, EachOtherDocked);
        WriteBool(cLeftDocked, LeftDocked);
        WriteBool(cTopDocked, TopDocked);
        WriteBool(cRightDocked, RightDocked);
        WriteBool(cBottomDocked, BottomDocked);
        WriteString(cDockClientData, DockClientData);
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
    if (Host <> nil) and (DockClientData <> '') then
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
    FJvDockInfoStyle := isWriteInfo;
    MiddleScanTree(TopTreeZone);
  finally
    FJvDockInfoStyle := isNone;
  end;
end;

{$ELSE}

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

  FJvDockInfoStyle := isWriteFileInfo;
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

{$ENDIF USEJVCL}

end.

