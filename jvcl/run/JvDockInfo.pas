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
}
{$I JVCL.INC}
unit JvDockInfo;

interface


uses Windows, Controls, Inifiles, Registry, Classes, Sysutils, Forms, Messages,
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
    ( isNone,         
      isReadFileInfo, 
      isWriteFileInfo,
      isReadRegInfo,  
      isWriteRegInfo);

  
  TJvDockInfoTree = class(TJvDockBaseTree)
  private
    FDockInfoIni: TIniFile;  
    FDockInfoReg: TRegistry; 
    FRegName: string;        
    FJvDockInfoStyle: TJvDockInfoStyle;
    FDataStream: TMemoryStream; 
    function FindDockForm(FormName: string): TCustomForm;
    function CreateHostControl(ATreeZone: TJvDockInfoZone): TWinControl;

  protected
    procedure ScanTreeZone(TreeZone: TJvDockBaseZone); override;    
    procedure CreateZoneAndAddInfoFromIni; virtual;
    procedure CreateZoneAndAddInfoFromReg; virtual;
    procedure SetDockControlInfo(ATreeZone: TJvDockInfoZone); virtual;
  public
    constructor Create(TreeZone: TJvDockTreeZoneClass); override;
    destructor Destroy; override;
    procedure CreateZoneAndAddInfoFromApp(Control: TControl); virtual;

    procedure ReadInfoFromIni;                    
    procedure ReadInfoFromReg(RegName: string);   
    procedure WriteInfoToIni;                     
    procedure WriteInfoToReg(RegName: string);    
    property DockInfoIni: TIniFile read FDockInfoIni write FDockInfoIni;
    property DockInfoReg: TRegistry read FDockInfoReg write FDockInfoReg;
  end;

implementation

uses JvDockGlobals, JvDockVSNetStyle;


function FindDockForm(FormName: string): TCustomForm;
begin
  if Pos(RsDockJvDockInfoSplitter, FormName) > 0 then Result := nil
  else
  begin
    Result := JvDockFindDockFormWithName(FormName);
  end;
end;

function FindDockPanel(ControlName: string): TWinControl;
var Index: Word;
  DockServer: TJvDockServer;
begin
  Result := nil;
  Index := Pos(RsDockJvDockInfoSplitter, ControlName);
  if Index = 0 then Exit;
  Result := FindDockForm(Copy(ControlName, 1, Index - 1));
  if Result <> nil then
  begin
    DockServer := FindDockServer(Result);
    if DockServer <> nil then
      with DockServer do
      begin
        if Pos('TopDockPanel', ControlName) > Index then
          Result := TopDockPanel
        else if Pos('LeftDockPanel', ControlName) > Index then
          Result := LeftDockPanel
        else if Pos('BottomDockPanel', ControlName) > Index then
          Result := BottomDockPanel
        else if Pos('RightDockPanel', ControlName) > Index then
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



function TJvDockInfoZone.GetChildControlCount: Integer;
var AZone: TJvDockBaseZone;
begin
  Result := 0;
  if ChildZone <> nil then
  begin
    Inc(Result);
    AZone := ChildZone;
    while AZone.NextSibling <> nil do
    begin
      AZone := AZone.NextSibling;
      if TJvDockInfoZone(AZone).DockControl <> nil then
        Inc(Result);
    end;
  end;
end;

procedure TJvDockInfoZone.SetDockInfoFromControlToNode(Control: TControl);
begin
  DockRect      := Control.BoundsRect;
  UnDockWidth   := Control.UndockWidth;
  UnDockHeight  := Control.UndockHeight;
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

procedure TJvDockInfoZone.SetDockInfoFromDockControlToNode(
  DockControl: TJvDockBaseControl);
  function GetLastDockSiteName(AControl: TControl): string;
  begin
    Result := RsDockCannotFindWindow;
    if AControl <> nil then
    begin
      if AControl.Parent is TJvDockableForm then
        Result := AControl.Parent.Name
      else if AControl is TJvDockPanel then
        Result := AControl.Parent.Name + RsDockJvDockInfoSplitter + AControl.Name;
    end;
  end;
begin
  CanDocked       := DockControl.EnableDock;
  EachOtherDocked := DockControl.EachOtherDock;
  LeftDocked      := DockControl.LeftDock;
  TopDocked       := DockControl.TopDock;
  RightDocked     := DockControl.RightDock;
  BottomDocked    := DockControl.BottomDock;
  if DockControl is TJvDockClient then
  begin
    VSPaneWidth   := TJvDockClient(DockControl).VSPaneWidth;
    UnDockLeft := TJvDockClient(DockControl).UnDockLeft;
    UnDockTop := TJvDockClient(DockControl).UnDockTop;
    LastDockSiteName := GetLastDockSiteName(TJvDockClient(DockControl).LastDockSite);
  end
  else
    VSPaneWidth   := 0;
end;

procedure TJvDockInfoZone.SetDockInfoFromNodeToControl(Control: TControl);
var lbDockServer: TJvDockServer;

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
      end else
      begin
        if Control is TJvDockVSPopupPanel then
          SetPopupPanelSize(Control as  TJvDockVSPopupPanel)
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

procedure TJvDockInfoZone.SetDockInfoFromNodeToDockControl(
  DockControl: TJvDockBaseControl);
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
  if (DockControl is TJvDockClient) then
  begin
    TJvDockClient(DockControl).UnDockLeft := UnDockLeft;
    TJvDockClient(DockControl).UnDockTop := UnDockTop;
    TJvDockClient(DockControl).LastDockSite := GetLastDockSite(LastDockSiteName);
    if Visible then
    begin
      TJvDockClient(DockControl).ParentVisible := False;
      TJvDockClient(DockControl).MakeShowEvent;
    end else TJvDockClient(DockControl).MakeHideEvent;
    TJvDockClient(DockControl).VSPaneWidth := VSPaneWidth;
  end;
  DockControl.EnableDock := CanDocked;
  DockControl.LeftDock := LeftDocked;
  DockControl.TopDock := TopDocked;
  DockControl.BottomDock := BottomDocked;
  DockControl.RightDock := RightDocked;
end;



constructor TJvDockInfoTree.Create(TreeZone: TJvDockTreeZoneClass);
begin
  inherited Create(TreeZone);
  FDockInfoIni := nil;
  FJvDockInfoStyle := isNone;
  FDataStream := TMemoryStream.Create;
end;

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromApp(Control: TControl);
var i: Integer;
  TreeZone: TJvDockInfoZone;
  ADockBaseControl: TJvDockBaseControl;
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
    else DockFormName := Control.Name;
    FDataStream.Clear;
    if Control is TJvDockTabHostForm then
      TJvDockTabHostForm(Control).PageControl.SaveToStream(FDataStream)
    else if Control is TJvDockConjoinHostForm then
      TJvDockConjoinHostForm(Control).Panel.DockManager.SaveToStream(FDataStream)
    else if Control is TJvDockPanel then
      TJvDockPanel(Control).DockManager.SaveToStream(FDataStream);
    DockClientData := JvDockStreamDataToString(FDataStream);
    ADockBaseControl := FindDockBaseControl(Control);
    if ADockBaseControl <> nil then
    begin
      SetDockInfoFromDockControlToNode(ADockBaseControl);
      if Control is TJvDockTabHostForm then
        DockFormStyle := dsTab
      else if Control is TJvDockConjoinHostForm then
        DockFormStyle := dsConjoin
      else DockFormStyle := dsNormal;
      if ADockBaseControl is TJvDockClient then
      begin
        if Control is TJvDockableForm then
        begin
          with TJvDockableForm(Control).DockableControl do
          begin
            for i := 0 to DockClientCount -1 do
            begin
              CurrTreeZone := TreeZone;
              CreateZoneAndAddInfoFromApp(DockClients[i]);
              CurrTreeZone := TreeZone.GetParentZone;
            end;
          end;
        end;
      end else
      begin
        for i := 0 to 3 do
        begin
          CurrTreeZone := TreeZone;
          TmpDockPanel := TJvDockServer(ADockBaseControl).DockPanel[i];
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
      begin
        with TJvDockVSPopupPanel(Control) do
        begin
          for i := 0 to DockClientCount - 1 do
          begin
            CurrTreeZone := TreeZone;
            CreateZoneAndAddInfoFromApp(TWinControl(DockClients[i]));
            CurrTreeZone := TreeZone.GetParentZone;
          end;
        end;
      end else
      with TJvDockPanel(Control) do
      begin
        for i := 0 to DockClientCount - 1 do
        begin
          CurrTreeZone := TreeZone;
          CreateZoneAndAddInfoFromApp(TWinControl(DockClients[i]));
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;
  end;
end;

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromIni;
var Sections: TStringList;
    TempDockInfoZoneArray: Array of TJvDockInfoZone;             

  
  procedure CreateTempDockInfoZoneArray;
  var i: Integer;
  begin
    SetLength(TempDockInfoZoneArray, Sizeof(TJvDockInfoZone) * Sections.Count);
    for i := 0 to Sections.Count - 1 do
    begin
      TempDockInfoZoneArray[i] := TJvDockInfoZone.Create(nil);
      with TempDockInfoZoneArray[i], FDockInfoIni do
      begin
        
        DockFormName  := Sections[i];
        ParentName    := ReadString(DockFormName, 'ParentName', 'ERROR');
        DockRect      := Rect(ReadInteger(DockFormName, 'DockLeft', 0),
                         ReadInteger(DockFormName, 'DockTop', 0),
                         ReadInteger(DockFormName, 'DockRight', 100),
                         ReadInteger(DockFormName, 'DockBottom', 100));
        LastDockSiteName:= ReadString(DockFormName, 'LastDockSiteName', 'ERROR');
        UnDockLeft    := ReadInteger(DockFormName, 'UnDockLeft', 100);
        UnDockTop     := ReadInteger(DockFormName, 'UnDockTop', 100);
        LRDockWidth   := ReadInteger(DockFormName, 'LRDockWidth', 100);
        TBDockHeight  := ReadInteger(DockFormName, 'TBDockHeight', 100);
        UnDockWidth   := ReadInteger(DockFormName, 'UndockWidth', 100);
        UnDockHeight  := ReadInteger(DockFormName, 'UndockHeight', 100);
        VSPaneWidth   := ReadInteger(DockFormName, 'VSPaneWidth', 100);
        Visible       := ReadBool(DockFormName, 'Visible', True);
        BorderStyle   := TBorderStyle(ReadInteger(DockFormName, 'BorderStyle', 0));
        FormStyle     := TFormStyle(ReadInteger(DockFormName, 'FormStyle', 0));
        WindowState   := TWindowState(ReadInteger(DockFormName, 'WindowState', 0));
        DockFormStyle := TJvDockFormStyle(ReadInteger(DockFormName, 'DockFormStyle', 0));
        CanDocked     := ReadBool(DockFormName, 'CanDocked', True);
        EachOtherDocked := ReadBool(DockFormName, 'EachOtherDocked', True);
        LeftDocked    := ReadBool(DockFormName, 'LeftDocked', LeftDocked);
        TopDocked     := ReadBool(DockFormName, 'TopDocked', True);
        RightDocked   := ReadBool(DockFormName, 'RightDocked', True);
        BottomDocked  := ReadBool(DockFormName, 'BottomDocked', True);
        DockClientData:= ReadString(DockFormName, 'DockClientData', '');
      end;
    end;
  end;

  
  procedure DestroyTempDockInfoZoneArray;
  var i: Integer;
  begin
    for i := Sections.Count - 1 downto 0 do
    begin
      TempDockInfoZoneArray[i].Free;
    end;
  end;

  procedure CreateZoneAndAddInfo(Index: Integer);
  var i: Integer;
    TreeZone: TJvDockInfoZone;
  begin
    TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));

    
    TreeZone.DockFormName     := TempDockInfoZoneArray[Index].DockFormName;
    TreeZone.ParentName       := TempDockInfoZoneArray[Index].ParentName;
    TreeZone.DockRect         := TempDockInfoZoneArray[Index].DockRect;
    TreeZone.LastDockSiteName := TempDockInfoZoneArray[Index].LastDockSiteName;
    TreeZone.UnDockLeft       := TempDockInfoZoneArray[Index].UnDockLeft;
    TreeZone.UnDockTop        := TempDockInfoZoneArray[Index].UnDockTop;
    TreeZone.LRDockWidth      := TempDockInfoZoneArray[Index].LRDockWidth;
    TreeZone.TBDockHeight     := TempDockInfoZoneArray[Index].TBDockHeight;
    TreeZone.UnDockWidth      := TempDockInfoZoneArray[Index].UnDockWidth;
    TreeZone.UnDockHeight     := TempDockInfoZoneArray[Index].UnDockHeight;
    TreeZone.VSPaneWidth      := TempDockInfoZoneArray[Index].VSPaneWidth;
    TreeZone.Visible          := TempDockInfoZoneArray[Index].Visible;
    TreeZone.BorderStyle      := TempDockInfoZoneArray[Index].BorderStyle;
    TreeZone.FormStyle        := TempDockInfoZoneArray[Index].FormStyle;
    TreeZone.WindowState      := TempDockInfoZoneArray[Index].WindowState;
    TreeZone.DockFormStyle    := TempDockInfoZoneArray[Index].DockFormStyle;
    TreeZone.CanDocked        := TempDockInfoZoneArray[Index].CanDocked;
    TreeZone.EachOtherDocked  := TempDockInfoZoneArray[Index].EachOtherDocked;
    TreeZone.LeftDocked       := TempDockInfoZoneArray[Index].LeftDocked;
    TreeZone.TopDocked        := TempDockInfoZoneArray[Index].TopDocked;
    TreeZone.RightDocked      := TempDockInfoZoneArray[Index].RightDocked;
    TreeZone.BottomDocked     := TempDockInfoZoneArray[Index].BottomDocked;
    TreeZone.DockClientData   := TempDockInfoZoneArray[Index].DockClientData;

    for i := Index - 1 downto 0 do
    begin
      if TempDockInfoZoneArray[i].ParentName = Sections[Index] then
      begin
        CurrTreeZone := TreeZone;
        CreateZoneAndAddInfo(i);
        CurrTreeZone := TreeZone.GetParentZone;
      end;
    end;
  end;

var i: Integer;
begin
  Sections := TStringList.Create;
  try
    FDockInfoIni.ReadSections(Sections);

    
    CreateTempDockInfoZoneArray;

    
    FJvDockInfoStyle := isReadFileInfo;

    for i := Sections.Count - 1 downto 0 do
    begin
      if TempDockInfoZoneArray[i].ParentName = '' then
        CreateZoneAndAddInfo(i);
    end;
    FJvDockInfoStyle := isNone;
  finally
    DestroyTempDockInfoZoneArray;
    Sections.Free;
  end;
end;

procedure TJvDockInfoTree.CreateZoneAndAddInfoFromReg;
var FormList: TStringList;
  procedure CreateZoneAndAddInfo(Index: Integer);
  var i: Integer;
    TreeZone: TJvDockInfoZone;
  begin
    FDockInfoReg.OpenKey(FRegName, False);
    if FDockInfoReg.KeyExists(FormList[Index]) then
    begin
      FDockInfoReg.OpenKey(FRegName + '\' + FormList[Index], False);
      TreeZone := TJvDockInfoZone(AddChildZone(CurrTreeZone, nil));
      with TreeZone, FDockInfoReg do
      begin
        
        DockFormName  := FormList[Index];
        ParentName    := ReadString('ParentName');
        DockRect := Rect(ReadInteger('DockLeft'),
                         ReadInteger('DockTop'),
                         ReadInteger('DockRight'),
                         ReadInteger('DockBottom'));
        LRDockWidth   := ReadInteger('LRDockWidth');
        LastDockSiteName:= ReadString('LastDockSiteName');
        UnDockLeft    := ReadInteger('UnDockLeft');
        UnDockTop     := ReadInteger('UnDockTop');
        TBDockHeight  := ReadInteger('TBDockHeight');
        UnDockWidth   := ReadInteger('UnDockWidth');
        UnDockHeight  := ReadInteger('UnDockHeight');
        VSPaneWidth   := ReadInteger('VSPaneWidth');
        Visible       := ReadBool('Visible');
        BorderStyle   := TBorderStyle(ReadInteger('BorderStyle'));
        FormStyle     := TFormStyle(ReadInteger('FormStyle'));
        WindowState   := TWindowState(ReadInteger('WindowState'));
        DockFormStyle := TJvDockFormStyle(ReadInteger('DockFormStyle'));
        CanDocked     := ReadBool('CanDocked');
        EachOtherDocked := ReadBool('EachOtherDocked');
        LeftDocked    := ReadBool('LeftDocked');
        TopDocked     := ReadBool('TopDocked');
        RightDocked   := ReadBool('RightDocked');
        BottomDocked  := ReadBool('BottomDocked');
        DockClientData:= ReadString('DockClientData');
      end;
      for i := Index - 1 downto 0 do
      begin
        FDockInfoReg.OpenKey(FRegName + '\' + FormList[i], False);
        if FDockInfoReg.ReadString('ParentName') = FormList[Index] then
        begin
          CurrTreeZone := TreeZone;
          CreateZoneAndAddInfo(i);
          CurrTreeZone := TreeZone.GetParentZone;
        end;
      end;
    end;
  end;
var
  cp, cp1: PChar;
  i: Integer;
begin
  FormList := TStringList.Create;
  try
    if FDockInfoReg.OpenKey(FRegName, False) then
    begin
      cp := PChar(FDockInfoReg.ReadString('FormNames'));
      cp1 := StrPos(cp, '\');
      while cp1 <> nil do
      begin
        cp1^ := #0;
        FormList.Add(string(cp));
        cp := cp1 + 1;
        cp1 := StrPos(cp, '\');
      end;
      FJvDockInfoStyle := isReadFileInfo;
      for i := FormList.Count - 1 downto 0 do
      begin
        FDockInfoReg.OpenKey(FRegName + '\' + FormList[i], False);
        if FDockInfoReg.ReadString('ParentName') = '' then
          CreateZoneAndAddInfo(i);
      end;
      FJvDockInfoStyle := isNone;
    end;
  finally
    FDockInfoReg.CloseKey;
    FormList.Free;
  end;
end;

destructor TJvDockInfoTree.Destroy;
begin
  FDataStream.Free;
  inherited Destroy;
end;

procedure TJvDockInfoTree.ReadInfoFromIni;
begin
  CreateZoneAndAddInfoFromIni;
  
  DoFloatAllForm;
  
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
  
  Application.ProcessMessages;
  
  FJvDockInfoStyle := isReadRegInfo;
  MiddleScanTree(TopTreeZone);
  FJvDockInfoStyle := isNone;
end;

procedure TJvDockInfoTree.ScanTreeZone(TreeZone: TJvDockBaseZone);
var i: Integer;
begin
  if (FJvDockInfoStyle = isReadFileInfo) or (FJvDockInfoStyle = isReadRegInfo) then
  begin
    
    for i := 0 to TreeZone.GetChildCount - 1 do
    begin
      with TJvDockInfoZone(TreeZone.GetChildZone(i)) do
        DockControl := FindDockForm(DockFormName);
    end;
    SetDockControlInfo(TJvDockInfoZone(TreeZone));
  end else
  if FJvDockInfoStyle = isWriteFileInfo then
  begin
    
    if TreeZone <> TopTreeZone then
    begin
      with TJvDockInfoZone(TreeZone), FDockInfoIni do
      begin
        WriteString(DockFormName,
          'ParentName', ParentName);
        WriteInteger(DockFormName,
          'DockLeft', DockRect.Left);
        WriteInteger(DockFormName,
          'DockTop', DockRect.Top);
        WriteInteger(DockFormName,
          'DockRight', DockRect.Right);
        WriteInteger(DockFormName,
          'DockBottom', DockRect.Bottom);
        WriteString(DockFormName,
          'LastDockSiteName', LastDockSiteName);
        WriteInteger(DockFormName,
          'UnDockLeft', UnDockLeft);
        WriteInteger(DockFormName,
          'UnDockTop', UnDockTop);
        WriteInteger(DockFormName,
          'LRDockWidth', LRDockWidth);
        WriteInteger(DockFormName,
          'TBDockHeight', TBDockHeight);
        WriteInteger(DockFormName,
          'UnDockWidth', UnDockWidth);
        WriteInteger(DockFormName,
          'UnDockHeight', UnDockHeight);
        WriteInteger(DockFormName,
          'VSPaneWidth', VSPaneWidth);
        WriteBool(DockFormName,
          'Visible', Visible);
        WriteInteger(DockFormName,
          'BorderStyle', Integer(BorderStyle));
        WriteInteger(DockFormName,
          'WindowState', Integer(WindowState));
        WriteInteger(DockFormName,
          'FormStyle', Integer(FormStyle));
        WriteInteger(DockFormName,
          'DockFormStyle', Integer(DockFormStyle));
        WriteBool(DockFormName,
          'CanDocked', CanDocked);
        WriteBool(DockFormName,
          'EachOtherDocked', EachOtherDocked);
        WriteBool(DockFormName,
          'LeftDocked', LeftDocked);
        WriteBool(DockFormName,
          'TopDocked', TopDocked);
        WriteBool(DockFormName,
          'RightDocked', RightDocked);
        WriteBool(DockFormName,
          'BottomDocked', BottomDocked);
        WriteString(DockFormName,
          'DockClientData', DockClientData);
      end;
    end;
  end else if FJvDockInfoStyle = isWriteRegInfo then
  begin
    if TreeZone <> TopTreeZone then
    begin
      with TJvDockInfoZone(TreeZone), FDockInfoReg do
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
  end;
  inherited ScanTreeZone(TreeZone);
end;


function TJvDockInfoTree.FindDockForm(FormName: string): TCustomForm;
begin
  if Pos(RsDockJvDockInfoSplitter, FormName) > 0 then Result := nil
  else
  begin
    Result := JvDockFindDockFormWithName(FormName);
  end;
end;





function TJvDockInfoTree.CreateHostControl(ATreeZone: TJvDockInfoZone): TWinControl;
var Form: TForm;
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
  begin
    Result.Name := ATreeZone.DockFormName;
  end;
end;

procedure TJvDockInfoTree.SetDockControlInfo(ATreeZone: TJvDockInfoZone);
var
  ADockBaseControl: TJvDockBaseControl;
  Host: TWinControl;
begin
  with ATreeZone do
  begin
    if DockFormName = '' then Exit;
    Host := FindDockHost(DockFormName);
    if (Host = nil) and (ATreeZone.GetChildControlCount > 1)  then
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
          try LoadFromStream(FDataStream);
          finally EnableAlign; end;
        end;
      end else if Host is TJvDockConjoinHostForm then
      begin
        
        with TJvDockConjoinHostForm(Host).Panel do
        begin
          DisableAlign;
          try DockManager.LoadFromStream(FDataStream);
          finally EnableAlign; end;
        end;
      end else if Host is TJvDockPanel then
      begin
        
        with TJvDockPanel(Host) do
        begin
          DisableAlign;
          try DockManager.LoadFromStream(FDataStream);
          finally EnableAlign; end;
        end;
      end;
    end;
    if Host <> nil then
    begin
      SetDockInfoFromNodeToControl(Host);
      ADockBaseControl := FindDockBaseControl(Host);
      if ADockBaseControl <> nil then
        SetDockInfoFromNodeToDockControl(ADockBaseControl);
    end;
  end;
end;

procedure TJvDockInfoTree.WriteInfoToIni;
var Sections: TStringList;
  i: Integer;
begin
  Sections := TStringList.Create;
  try
    FDockInfoIni.ReadSections(Sections);
    
    for i := 0 to Sections.Count - 1 do
      FDockInfoIni.EraseSection(Sections[i]);
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
    
    if FDockInfoReg.OpenKey(RegName, False) then
      FDockInfoReg.DeleteKey(RegName);
    
    FDockInfoReg.CreateKey(RegName);
    FDockInfoReg.CloseKey;
    FRegName := RegName;
    
    FJvDockInfoStyle := isWriteRegInfo;
    MiddleScanTree(TopTreeZone);
    FJvDockInfoStyle := isNone;
  finally
    FDockInfoReg.CloseKey;
  end;
end;

end.

