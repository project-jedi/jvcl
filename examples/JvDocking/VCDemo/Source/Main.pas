{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$I jvcl.inc}
unit Main;
{$IFNDEF USEJVCL}
Sorry, this demo requires the JVCL!
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ToolWin, ImgList, ExtCtrls, StdCtrls, Menus, ActnList,
  JvDockControlForm, JvDockVCStyle, Grids, StdActns, JVHLEditor,
  JvDockDelphiStyle, JvDockVIDStyle
  {$IFDEF USEJVCL}
  , JvComponent, JvAppStorage, JvAppIniStorage
  {$ENDIF};

type
  TMainForm = class(TForm)
    MainControlBar: TControlBar;
    tb_Standard_ToolBar: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    WorkSpace_ToolButton: TToolButton;
    Output_ToolButton: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    Search_Panel: TPanel;
    ComboBox1: TComboBox;
    tb_Edit_ToolBar: TToolBar;
    ToolButton19: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    tb_Build_MiniBar_ToolBar: TToolBar;
    ToolButton29: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton34: TToolButton;
    ToolButton36: TToolButton;
    tb_Debug_ToolBar: TToolBar;
    ToolButton37: TToolButton;
    ToolButton38: TToolButton;
    ToolButton39: TToolButton;
    ToolButton40: TToolButton;
    ToolButton41: TToolButton;
    ToolButton42: TToolButton;
    ToolButton43: TToolButton;
    ToolButton44: TToolButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Edit1: TMenuItem;
    View1: TMenuItem;
    Insert1: TMenuItem;
    Project1: TMenuItem;
    Build1: TMenuItem;
    Tools1: TMenuItem;
    Window1: TMenuItem;
    Help1: TMenuItem;
    MainMenu_ToolBar: TToolBar;
    New1: TMenuItem;
    Open1: TMenuItem;
    Open2: TMenuItem;
    N1: TMenuItem;
    OpenWorkSpace1: TMenuItem;
    MainActionList: TActionList;
    File_New_Action: TAction;
    File_Open_Action: TAction;
    File_Close_Action: TAction;
    File_Open_WorkSpace_Action: TAction;
    File_Save_WorKSpace_Action: TAction;
    File_Close_WorKSpace_Action: TAction;
    File_Save_Action: TAction;
    File_SaveAs_Action: TAction;
    File_SaveAll_Action: TAction;
    File_Page_Setup_Action: TAction;
    File_Print_Action: TAction;
    File_Recent_Files_Action: TAction;
    File_Recent_WorkSpace_Action: TAction;
    File_Exit_Action: TAction;
    SaveWorkSpace1: TMenuItem;
    CloseWorkSpace1: TMenuItem;
    N2: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    SaveAll1: TMenuItem;
    N3: TMenuItem;
    PageSetup1: TMenuItem;
    Print1: TMenuItem;
    N4: TMenuItem;
    RecentFiles1: TMenuItem;
    RecentWorkSpace1: TMenuItem;
    N5: TMenuItem;
    Exit1: TMenuItem;
    Action_ImageList: TImageList;
    ToolButton45: TToolButton;
    ToolButton46: TToolButton;
    ToolButton47: TToolButton;
    ToolButton48: TToolButton;
    Watch_ToolButton: TToolButton;
    Variables_ToolButton: TToolButton;
    Registers_ToolButton: TToolButton;
    Memory_ToolButton: TToolButton;
    CallStack_ToolButton: TToolButton;
    ToolButton54: TToolButton;
    ToolButton55: TToolButton;
    Edit_Redo_Action: TAction;
    Edit_Find_Action: TAction;
    Edit_Find_In_File_Action: TAction;
    Edit_Replace_Action: TAction;
    Edit_Bookmarks_Action: TAction;
    Redo1: TMenuItem;
    Redo2: TMenuItem;
    N6: TMenuItem;
    Cut1: TMenuItem;
    Cut2: TMenuItem;
    Paste1: TMenuItem;
    Delete1: TMenuItem;
    N7: TMenuItem;
    SelectAll1: TMenuItem;
    N8: TMenuItem;
    Find1: TMenuItem;
    FindInFile1: TMenuItem;
    Replace1: TMenuItem;
    N9: TMenuItem;
    GoTo1: TMenuItem;
    View_Resource_Symbols_Action: TAction;
    View_Resource_Include_Action: TAction;
    View_Full_Screen_Action: TAction;
    View_Workspace_Action: TAction;
    View_OutPut_Action: TAction;
    View_Refresh_Action: TAction;
    View_Properties_Action: TAction;
    View_Debug_Windows_Action: TAction;
    ResourceSymbols1: TMenuItem;
    ResourceInclude1: TMenuItem;
    N10: TMenuItem;
    FullScreen1: TMenuItem;
    N11: TMenuItem;
    Workspace1: TMenuItem;
    OutPut1: TMenuItem;
    N12: TMenuItem;
    Refresh1: TMenuItem;
    N13: TMenuItem;
    Properties1: TMenuItem;
    Edit_Advanced_Action: TAction;
    Edit_Breakpoints_Action: TAction;
    Edit_List_Members_Action: TAction;
    Edit_Type_Info_Action: TAction;
    Edit_Parameter_Info_Action: TAction;
    Edit_Complete_Word_Action: TAction;
    Edit_Goto_Action: TAction;
    GoTo2: TMenuItem;
    N14: TMenuItem;
    Advanced1: TMenuItem;
    N15: TMenuItem;
    Breakpoints1: TMenuItem;
    N16: TMenuItem;
    ListMembers1: TMenuItem;
    ypeInfo1: TMenuItem;
    ParameterInfo1: TMenuItem;
    CompleteWord1: TMenuItem;
    Insert_New_Class_Action: TAction;
    Insert_New_Form_Action: TAction;
    Insert_Resource_Action: TAction;
    Insert_Resource_Copy_Action: TAction;
    Insert_File_As_Text_Action: TAction;
    Insert_New_ATL_Object_Action: TAction;
    Project_Set_Active_Project_Action: TAction;
    Project_Add_To_Project_Action: TAction;
    Project_Source_Control_Action: TAction;
    Project_DePendencies_Action: TAction;
    Project_Settings_Action: TAction;
    Project_Export_Makefile_Action: TAction;
    Project_Insert_Project_into_Workspace_Action: TAction;
    Build_Compile_Action: TAction;
    Build_Build_Action: TAction;
    Build_Rebuild_All_Action: TAction;
    Build_Batch_Build_Action: TAction;
    Build_Clean_Action: TAction;
    Build_Start_Debug_Action: TAction;
    Build_Debugger_Remote_Connection_Action: TAction;
    Build_Execute_Action: TAction;
    Build_Set_Active_Configuration_Action: TAction;
    Build_Configurations_Action: TAction;
    Build_Profile_Action: TAction;
    Tools_Source_Browser_Action: TAction;
    Tools_Close_Source_Browser_File_Action: TAction;
    Tools_Visual_Component_Manager_Action: TAction;
    Tools_Register_Control_Action: TAction;
    Tools_Error_Lookup_Action: TAction;
    Tools_ActiveX_Control_Test_Container_Action: TAction;
    Tools_OLE_COM_Object_Viewer_Action: TAction;
    Tools_Spy_Action: TAction;
    Tools_MFC_Tracer_Action: TAction;
    Tools_Customize_Action: TAction;
    Tools_Options_Action: TAction;
    Tools_Macro_Action: TAction;
    Tools_Record_Quick_Macro_Action: TAction;
    Tools_Play_Quick_Macro_Action: TAction;
    Window_New_Window_Action: TAction;
    Window_Spilt_Action: TAction;
    Window_Docking_View_Action: TAction;
    Window_Close_Action: TAction;
    Window_Close_All_Action: TAction;
    Window_Next_Action: TAction;
    Window_Previous_Action: TAction;
    Window_Cascade_Action: TAction;
    Window_Tile_Horizontally_Action: TAction;
    Window_Tile_Vertically_Action: TAction;
    Window_Windows_Action: TAction;
    Help_Contents_Action: TAction;
    Help_Search_Action: TAction;
    Help_Index_Action: TAction;
    Help_Use_Extension_Help_Action: TAction;
    Help_Keyboard_Map_Action: TAction;
    Help_Tip_of_the_Day_Action: TAction;
    Help_Technical_Support_Action: TAction;
    Help_Microsofr_on_the_Web_Action: TAction;
    Help_About_Visual_Cpp_Action: TAction;
    NewClass1: TMenuItem;
    NewForm1: TMenuItem;
    Resource1: TMenuItem;
    ResourceCopy1: TMenuItem;
    N17: TMenuItem;
    FileAsText1: TMenuItem;
    NewATLObject1: TMenuItem;
    SetActiveProject1: TMenuItem;
    AddToProject1: TMenuItem;
    N18: TMenuItem;
    SourceControl1: TMenuItem;
    N19: TMenuItem;
    DePendencies1: TMenuItem;
    Settings1: TMenuItem;
    DePendencies2: TMenuItem;
    N20: TMenuItem;
    InsertProjectintoWorkspace1: TMenuItem;
    Compile1: TMenuItem;
    Build2: TMenuItem;
    RebuildAll1: TMenuItem;
    BatchBuild1: TMenuItem;
    Clean1: TMenuItem;
    N21: TMenuItem;
    StartDebug1: TMenuItem;
    DebuggerRemoteConnection1: TMenuItem;
    N22: TMenuItem;
    Execute1: TMenuItem;
    N23: TMenuItem;
    SetActiveConfiguration1: TMenuItem;
    Configurations1: TMenuItem;
    Profile1: TMenuItem;
    SourceBrowser1: TMenuItem;
    CloseSourceBrowserFile1: TMenuItem;
    N24: TMenuItem;
    VisualComponentManager1: TMenuItem;
    ActiveXControlTestContainer1: TMenuItem;
    ErrorLookup1: TMenuItem;
    ActiveXControlTestContainer2: TMenuItem;
    OLECOMObjectViewer1: TMenuItem;
    SourceBrowser2: TMenuItem;
    MFCTracer1: TMenuItem;
    N25: TMenuItem;
    Customize1: TMenuItem;
    Options1: TMenuItem;
    Macro1: TMenuItem;
    RecordQuickMacro1: TMenuItem;
    PlayQuickMacro1: TMenuItem;
    N26: TMenuItem;
    NewWindow1: TMenuItem;
    Spilt1: TMenuItem;
    DockingView1: TMenuItem;
    Close1: TMenuItem;
    CloseAll1: TMenuItem;
    NewWindow2: TMenuItem;
    Previous1: TMenuItem;
    ileHorizontally1: TMenuItem;
    ileVertically1: TMenuItem;
    Windows1: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    Contents1: TMenuItem;
    Search1: TMenuItem;
    Index1: TMenuItem;
    UseExtensionHelp1: TMenuItem;
    KeyboardMap1: TMenuItem;
    echnicalSupport1: TMenuItem;
    echnicalSupport2: TMenuItem;
    MicrosofrontheWeb1: TMenuItem;
    AboutVisualC1: TMenuItem;
    N30: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    Debug1: TMenuItem;
    Debug_Go_Action: TAction;
    Debug_Restart_Action: TAction;
    Debug_Stop_Debugging_Action: TAction;
    Debug_Break_Action: TAction;
    Debug_Apply_Code_Changes_Action: TAction;
    Debug_Step_Into_Action: TAction;
    Debug_Step_Over_Action: TAction;
    Debug_Step_Out_Action: TAction;
    Debug_Run_to_Cursor_Action: TAction;
    Debug_Step_Into_Specific_Function_Action: TAction;
    Debug_Exceptions_Action: TAction;
    Debug_Threads_Action: TAction;
    Debug_Modules_Action: TAction;
    Debug_Show_Next_Statement_Action: TAction;
    Debug_QuickWatch_Action: TAction;
    Go1: TMenuItem;
    Restart1: TMenuItem;
    StopDebugging1: TMenuItem;
    Break1: TMenuItem;
    ApplyCodeChanges1: TMenuItem;
    StepInto1: TMenuItem;
    StepOver1: TMenuItem;
    StepOut1: TMenuItem;
    RuntoCursor1: TMenuItem;
    StepIntoSpecificFunction1: TMenuItem;
    Exceptions1: TMenuItem;
    hreads1: TMenuItem;
    Modules1: TMenuItem;
    ShowNextStatement1: TMenuItem;
    QuickWatch1: TMenuItem;
    N33: TMenuItem;
    N34: TMenuItem;
    N35: TMenuItem;
    Cascade1: TMenuItem;
    Edit_Toggle_Bookmark_Action: TAction;
    Edit_Next_Bookmark_Action: TAction;
    Edit_Previous_Bookmark_Action: TAction;
    Edit_Clear_All_BookMarks_Action: TAction;
    Edit_Increase_Indent_Action: TAction;
    Edit_Decrease_Indent_Action: TAction;
    Edit_Toggle_WhiteSpace_Display_Action: TAction;
    View_Watch_Action: TAction;
    View_Variables_Action: TAction;
    View_Registers_Action: TAction;
    View_Memory_Action: TAction;
    View_CallStack_Action: TAction;
    View_Disassembly_Action: TAction;
    DebugWindows1: TMenuItem;
    Watch1: TMenuItem;
    CallStack1: TMenuItem;
    Memory1: TMenuItem;
    Variable1: TMenuItem;
    Register1: TMenuItem;
    Disassembly1: TMenuItem;
    Build_Stop_Build_Action: TAction;
    Build_Insert_Remove_Breakpoint_Action: TAction;
    JvDockVCStyle1: TJvDockVCStyle;
    OpenDialog1: TOpenDialog;
    StatusBar1: TStatusBar;
    lbDockServer1: TJvDockServer;
    Edit_Cut_Action: TEditCut;
    Edit_Copy_Action: TEditCopy;
    Edit_Paste_Action: TEditPaste;
    Edit_SelectAll_Action: TEditSelectAll;
    Edit_Undo_Action: TEditUndo;
    Edit_Delete_Action: TEditDelete;
    ViewPopupMenu: TPopupMenu;
    Output_PopupItem: TMenuItem;
    Watch_PopupItem: TMenuItem;
    Variables_PopupItem: TMenuItem;
    Registers_PopupItem: TMenuItem;
    Memory_PopupItem: TMenuItem;
    CallStack_PopupItem: TMenuItem;
    Workspace_PopupItem: TMenuItem;
    N36: TMenuItem;
    View_Standard_Toolbar_Action: TAction;
    View_Debug_Toolbar_Action: TAction;
    View_Build_Minibar_Toolbar_Action: TAction;
    View_Edit_Toolbar_Action: TAction;
    Standard1: TMenuItem;
    BuildMinibar1: TMenuItem;
    Edit2: TMenuItem;
    Debug2: TMenuItem;
    View_Customize_Toolbar_Action: TAction;
    N37: TMenuItem;
    BuildMinibar2: TMenuItem;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton33: TToolButton;
    ToolButton35: TToolButton;
    ToolButton49: TToolButton;
    ToolButton50: TToolButton;
    ToolButton51: TToolButton;
    ToolButton52: TToolButton;
    ToolButton53: TToolButton;
    procedure View_Workspace_ActionExecute(Sender: TObject);
    procedure View_OutPut_ActionExecute(Sender: TObject);
    procedure MainControlBarBandMove(Sender: TObject; Control: TControl;
      var ARect: TRect);
    procedure File_New_ActionExecute(Sender: TObject);
    procedure File_Open_ActionExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure MainControlBarResize(Sender: TObject);
    procedure MainMenu_ToolBarCustomDraw(Sender: TToolBar;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure MainMenu_ToolBarMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MainMenu_ToolBarMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure MainMenu_ToolBarMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure View_Watch_ActionExecute(Sender: TObject);
    procedure Window_Cascade_ActionExecute(Sender: TObject);
    procedure Window_Tile_Horizontally_ActionExecute(Sender: TObject);
    procedure Window_Tile_Vertically_ActionExecute(Sender: TObject);
    procedure View_Registers_ActionExecute(Sender: TObject);
    procedure Window_Next_ActionExecute(Sender: TObject);
    procedure Window_Previous_ActionExecute(Sender: TObject);
    procedure View_Memory_ActionExecute(Sender: TObject);
    procedure Window_Close_ActionExecute(Sender: TObject);
    procedure Window_Close_All_ActionExecute(Sender: TObject);
    procedure File_Exit_ActionExecute(Sender: TObject);
    procedure Edit_Redo_ActionExecute(Sender: TObject);
    procedure Edit_Redo_ActionUpdate(Sender: TObject);
    procedure Edit_Cut_ActionUpdate(Sender: TObject);
    procedure Edit_Copy_ActionUpdate(Sender: TObject);
    procedure Edit_Paste_ActionUpdate(Sender: TObject);
    procedure Edit_Undo_ActionUpdate(Sender: TObject);
    procedure Edit_Delete_ActionUpdate(Sender: TObject);
    procedure Edit_Cut_ActionExecute(Sender: TObject);
    procedure Edit_Copy_ActionExecute(Sender: TObject);
    procedure Edit_Paste_ActionExecute(Sender: TObject);
    procedure Edit_SelectAll_ActionExecute(Sender: TObject);
    procedure Edit_Undo_ActionExecute(Sender: TObject);
    procedure Edit_Delete_ActionExecute(Sender: TObject);
    procedure lbDockServer1GetClientAlignSize(Align: TAlign;
      var Value: Integer);
    procedure View_Standard_Toolbar_ActionExecute(Sender: TObject);
    procedure View_Debug_Toolbar_ActionExecute(Sender: TObject);
    procedure View_Build_Minibar_Toolbar_ActionExecute(Sender: TObject);
    procedure View_Edit_Toolbar_ActionExecute(Sender: TObject);
    procedure View_Variables_ActionExecute(Sender: TObject);
    procedure View_Debug_Windows_ActionExecute(Sender: TObject);
    procedure View_CallStack_ActionExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    {$IFDEF USEJVCL}
    JvAppStorage:TJvAppIniFileStorage;
    {$ENDIF}
    procedure GetToolbarWidthArr;
    function GetCloseButtonRect: TRect;
    function ActionEnable: Boolean;
    procedure SetToolButtonDownAndActionCheck;

    function FindExistFile(AFileName: string): TCustomForm;
    function FindOrOpenFile(filename:String):TCustomForm;

    { Private declarations }
  public
    { Public declarations }
    ActiveEdit: TJvHLEditor;
    procedure LoadDockInfo;
  end;

var
  MainForm: TMainForm;

implementation

uses WorkSpaceUnit, OutputUnit, SourceEditUnit, WatchUnit, RegistersUnit,
  MemoryUnit, VariablesUnit, CallStackUnit;

var
  DrawButtonRect: TRect;
  IsDown: Boolean;
  MouseDownButton: Integer;

{$R *.dfm}

procedure CreateDockableForm;
begin
  WorkSpaceForm := TWorkSpaceForm.Create(nil);//创建WorkSpace工具窗体
  OutputForm := TOutputForm.Create(nil);      //创建Output工具窗体
  WatchForm := TWatchForm.Create(nil);        //创建Watch工具窗体
  VariablesForm := TVariablesForm.Create(nil);//创建Variables工具窗体
  RegistersForm := TRegistersForm.Create(nil);//创建Registers工具窗体
  MemoryForm := TMemoryForm.Create(nil);      //创建Memory工具窗体
  CallStackForm := TCallStackForm.Create(nil);//创建CallStack工具窗体
end;

procedure TMainForm.SetToolButtonDownAndActionCheck;
begin
  View_Workspace_Action.Checked := WorkSpaceForm.Visible;
  WorkSpace_ToolButton.Down := WorkSpaceForm.Visible;

  View_Output_Action.Checked := OutputForm.Visible;
  Output_ToolButton.Down := OutputForm.Visible;

  View_Watch_Action.Checked := WatchForm.Visible;
  Watch_ToolButton.Down := WatchForm.Visible;

  View_Variables_Action.Checked := VariablesForm.Visible;
  Variables_ToolButton.Down := VariablesForm.Visible;

  View_Registers_Action.Checked := RegistersForm.Visible;
  Registers_ToolButton.Down := RegistersForm.Visible;

  View_Memory_Action.Checked := MemoryForm.Visible;
  Memory_ToolButton.Down := MemoryForm.Visible;

  View_CallStack_Action.Checked := CallStackForm.Visible;
  CallStack_ToolButton.Down := CallStackForm.Visible;
  { -------------------------------------------------------------------------- }
  View_Standard_Toolbar_Action.Checked := tb_Standard_Toolbar.Visible;
  View_Debug_Toolbar_Action.Checked := tb_Debug_Toolbar.Visible;
  View_Build_Minibar_Toolbar_Action.Checked := tb_Build_Minibar_Toolbar.Visible;
  View_Edit_Toolbar_Action.Checked := tb_Edit_Toolbar.Visible;

end;

procedure TMainForm.GetToolbarWidthArr;
var i: Integer;
begin
  for i := 0 to MainControlBar.ControlCount - 1 do
    MainControlBar.Controls[i].Tag := MainControlBar.Controls[i].Width;
end;

procedure TMainForm.View_Workspace_ActionExecute(Sender: TObject);
begin
  WorkSpace_ToolButton.Down := not WorkSpaceForm.Visible;
  View_WorkSpace_Action.Checked := WorkSpace_ToolButton.Down;
  if WorkSpace_ToolButton.Down then
    ShowDockForm(WorkSpaceForm)
  else WorkSpaceForm.Close;
end;

procedure TMainForm.View_OutPut_ActionExecute(Sender: TObject);
begin
  Output_ToolButton.Down := not OutputForm.Visible;
  View_Output_Action.Checked := Output_ToolButton.Down;
  if Output_ToolButton.Down then
    ShowDockForm(OutputForm)
  else OutputForm.Close;
end;

procedure TMainForm.MainControlBarBandMove(Sender: TObject;
  Control: TControl; var ARect: TRect);
begin
  if (Control.Tag <> ARect.Right - ARect.Left - 13)
    and (Control <> MainMenu_ToolBar) then
  begin
    ARect.Right := ARect.Left + Control.Tag + 13;
  end;
end;

procedure TMainForm.File_New_ActionExecute(Sender: TObject);
begin
  { 没事做，显示一个提示信息 }
  ShowMessage(Format('You have click ''%s''', [TAction(Sender).Caption]));
end;


function TMainForm.FindExistFile(AFileName: string): TCustomForm;
  var i: Integer;
  begin
    Result := nil;
    for i := 0 to MDIChildCount - 1 do
      if MDIChildren[i].Caption = AFileName then
      begin
        Result := MDIChildren[i];
        Exit;
      end;
  end;

function TMainForm.FindOrOpenFile(filename:String):TCustomForm;
var
  ExistForm: TCustomForm;
begin
        ExistForm := FindExistFile(filename);
        if ExistForm = nil then
        begin
          with TSourceEditForm.Create(nil) do
          begin
            try
              LoadFromFile(filename);
            except
              Release;
              ShowMessage('Open file Failed!');
            end;
          end;
        end else if not ExistForm.Active then begin
          ExistForm.Show;
        end;
end;

procedure TMainForm.File_Open_ActionExecute(Sender: TObject);
var
  i: Integer;

begin
  if OpenDialog1.Execute then
  begin
    if (OpenDialog1.Files.Count > 0) then
    begin
      if MDIChildCount + OpenDialog1.Files.Count >= 100 then
      begin
        ShowMessage('Files are too large to show, there are 100 files can be displayed at most');
        for i := OpenDialog1.Files.Count - 1 downto 100 - MDIChildCount do
          OpenDialog1.Files.Delete(i);
      end;

      for i := 0 to OpenDialog1.Files.Count - 1 do
      begin
          FindOrOpenFile(OpenDialog1.Files[i]);
      end;
    end;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
var i: Integer;
begin
  for i := MDIChildCount - 1 downto 0 do
    MDIChildren[i].Close;
  {$IFDEF USEJVCL}
  JvAppStorage.FileName := ExtractFilePath(Application.ExeName) + 'DockInfo.ini';
  SaveDockTreeToAppStorage(JvAppStorage);
  {$ELSE}
  SaveDockTreeToFile(ExtractFilePath(Application.ExeName) + 'DockInfo.ini');
  {$ENDIF}
end;

procedure TMainForm.MainControlBarResize(Sender: TObject);
begin
//  if (MainMenu_ToolBar.Width <> MainControlBar.ClientWidth - 13) then
  begin
    MainMenu_ToolBar.Width := MainControlBar.ClientWidth - 13;
    MainMenu_ToolBar.Invalidate;
  end;
end;

procedure TMainForm.MainMenu_ToolBarCustomDraw(Sender: TToolBar;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  if (MDIChildCount > 0) and (MDIChildren[0].WindowState = wsMaximized) then
  begin
    MainMenu_ToolBar.Canvas.FillRect(DrawButtonRect);
    DrawButtonRect := GetCloseButtonRect;

    DrawFrameControl(MainMenu_ToolBar.Canvas.Handle, DrawButtonRect, DFC_CAPTION, DFCS_CAPTIONCLOSE or Integer(IsDown and (MouseDownButton = 3)) * DFCS_PUSHED);
    OffsetRect(DrawButtonRect, - 18, 0);
    DrawFrameControl(MainMenu_ToolBar.Canvas.Handle, DrawButtonRect, DFC_CAPTION, DFCS_CAPTIONRESTORE or Integer(IsDown and (MouseDownButton = 2)) * DFCS_PUSHED);
    OffsetRect(DrawButtonRect, - 16, 0);
    DrawFrameControl(MainMenu_ToolBar.Canvas.Handle, DrawButtonRect, DFC_CAPTION, DFCS_CAPTIONMIN or Integer(IsDown and (MouseDownButton = 1)) * DFCS_PUSHED);
  end;
end;

procedure TMainForm.MainMenu_ToolBarMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
  P := Point(X, Y);
  DrawButtonRect := GetCloseButtonRect;
  DrawButtonRect.Left := DrawButtonRect.Left - 34;
  if PtInRect(DrawButtonRect, P) and (MDIChildCount > 0) and (MDIChildren[0].WindowState = wsMaximized) then
  begin
    IsDown := True;
    MainMenu_ToolBarMouseMove(Sender, Shift, X, Y);
  end;
end;

procedure TMainForm.MainMenu_ToolBarMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var p: TPoint;
  OldMouseDownButton: Integer;
begin
  if not IsDown then Exit;
  OldMouseDownButton := MouseDownButton;
  MouseDownButton := 0;
  P := Point(X, Y);
  DrawButtonRect := GetCloseButtonRect;

  if PtInRect(DrawButtonRect, P) and (MouseDownButton = 0) then
    MouseDownButton := 3;

  OffsetRect(DrawButtonRect, - 18, 0);
  if PtInRect(DrawButtonRect, P) and (MouseDownButton = 0) then
    MouseDownButton := 2;

  OffsetRect(DrawButtonRect, - 16, 0);
  if PtInRect(DrawButtonRect, P) and (MouseDownButton = 0) then
    MouseDownButton := 1;

  if MouseDownButton <> OldMouseDownButton then
    MainMenu_ToolBar.Invalidate;
end;

procedure TMainForm.MainMenu_ToolBarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var p: TPoint;
begin
  if (not IsDown) or (MDIChildCount = 0) then Exit;
  IsDown := False;
  MouseDownButton := 0;
  P := Point(X, Y);
  DrawButtonRect := GetCloseButtonRect;

  if PtInRect(DrawButtonRect, P) and (MouseDownButton = 0) then
    MouseDownButton := 3;

  OffsetRect(DrawButtonRect, - 18, 0);
  if PtInRect(DrawButtonRect, P) and (MouseDownButton = 0) then
    MouseDownButton := 2;

  OffsetRect(DrawButtonRect, - 16, 0);
  if PtInRect(DrawButtonRect, P) and (MouseDownButton = 0) then
    MouseDownButton := 1;

  if MouseDownButton <> 0 then
  begin
    case MouseDownButton of
      1: MDIChildren[0].WindowState := wsMinimized;
      2: MDIChildren[0].WindowState := wsNormal;
      3: MDIChildren[0].Close;
    end;
    MainMenu_ToolBar.Invalidate;
  end;
  MouseDownButton := 0;
end;

function TMainForm.GetCloseButtonRect: TRect;
begin
  Result := Rect(MainMenu_ToolBar.Width - 16,
                 5,
                 MainMenu_ToolBar.Width,
                 19);
end;

procedure TMainForm.View_Watch_ActionExecute(Sender: TObject);
begin
  Watch_ToolButton.Down := not WatchForm.Visible;
  View_Watch_Action.Checked := Watch_ToolButton.Down;
  if Watch_ToolButton.Down then
    ShowDockForm(WatchForm)
  else WatchForm.Close;
end;

procedure TMainForm.Window_Cascade_ActionExecute(Sender: TObject);
begin
  Cascade;
end;

procedure TMainForm.Window_Tile_Horizontally_ActionExecute(
  Sender: TObject);
begin
  TileMode := tbHorizontal;
  Tile;
end;

procedure TMainForm.Window_Tile_Vertically_ActionExecute(Sender: TObject);
begin
  TileMode := tbVertical;
  Tile;
end;

procedure TMainForm.View_Registers_ActionExecute(Sender: TObject);
begin
  Registers_ToolButton.Down := not RegistersForm.Visible;
  View_Registers_Action.Checked := Registers_ToolButton.Down;
  if Registers_ToolButton.Down then
    ShowDockForm(RegistersForm)
  else RegistersForm.Close;
end;

procedure TMainForm.Window_Next_ActionExecute(Sender: TObject);
begin
  Next;
end;

procedure TMainForm.Window_Previous_ActionExecute(Sender: TObject);
begin
  Previous;
end;

procedure TMainForm.View_Memory_ActionExecute(Sender: TObject);
begin
  Memory_ToolButton.Down := not MemoryForm.Visible;
  View_Memory_Action.Checked := Memory_ToolButton.Down;
  if Memory_ToolButton.Down then
    ShowDockForm(MemoryForm)
  else MemoryForm.Close;
end;

procedure TMainForm.Window_Close_ActionExecute(Sender: TObject);
begin
  if MDIChildCount > 0 then
    MDIChildren[0].Close;
end;

procedure TMainForm.Window_Close_All_ActionExecute(Sender: TObject);
var i: Integer;
begin
  for i := MDIChildCount - 1 downto 0 do
    MDIChildren[i].Close;
end;

procedure TMainForm.File_Exit_ActionExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.Edit_Redo_ActionExecute(Sender: TObject);
begin
  if (MDIChildCount > 0) and (ActiveEdit <> nil) then
    ActiveEdit.Redo;
end;

procedure TMainForm.Edit_Redo_ActionUpdate(Sender: TObject);
begin
  Edit_Redo_Action.Enabled := ActionEnable and ActiveEdit.CanRedo;
end;

procedure TMainForm.Edit_Cut_ActionUpdate(Sender: TObject);
begin
  Edit_Cut_Action.Enabled := ActionEnable and (ActiveEdit.SelText <> '');
end;

procedure TMainForm.Edit_Copy_ActionUpdate(Sender: TObject);
begin
  Edit_Copy_Action.Enabled := ActionEnable
end;

function TMainForm.ActionEnable: Boolean;
begin
  Result := (MDIChildCount > 0) and
    (MDIChildren[0].WindowState <> wsMinimized)
    and (ActiveEdit <> nil);
end;

procedure TMainForm.Edit_Paste_ActionUpdate(Sender: TObject);
begin
  Edit_Paste_Action.Enabled := ActionEnable and ActiveEdit.CanPaste;
end;

procedure TMainForm.Edit_Undo_ActionUpdate(Sender: TObject);
begin
  Edit_Undo_Action.Enabled := ActionEnable and ActiveEdit.CanUndo;
end;

procedure TMainForm.Edit_Delete_ActionUpdate(Sender: TObject);
begin
  Edit_Undo_Action.Enabled := ActionEnable;
end;

procedure TMainForm.Edit_Cut_ActionExecute(Sender: TObject);
begin
  if ActiveEdit <> nil then
    ActiveEdit.ClipboardCut;
end;

procedure TMainForm.Edit_Copy_ActionExecute(Sender: TObject);
begin
  if ActiveEdit <> nil then
    ActiveEdit.ClipboardCopy;
end;

procedure TMainForm.Edit_Paste_ActionExecute(Sender: TObject);
begin
  if ActiveEdit <> nil then
    ActiveEdit.ClipboardPaste;
end;

procedure TMainForm.Edit_SelectAll_ActionExecute(Sender: TObject);
begin
  if ActiveEdit <> nil then
    ActiveEdit.SelectAll;
end;

procedure TMainForm.Edit_Undo_ActionExecute(Sender: TObject);
begin
  if ActiveEdit <> nil then
    ActiveEdit.Undo;
end;

procedure TMainForm.Edit_Delete_ActionExecute(Sender: TObject);
begin
  if ActiveEdit <> nil then
    ActiveEdit.SelText := '';
end;

procedure TMainForm.lbDockServer1GetClientAlignSize(Align: TAlign;
  var Value: Integer);
begin
  if Align = alBottom then
  begin
//    DockPanel.Top := 0;
    StatusBar1.Top := Height - StatusBar1.Height;
  end;
end;

procedure TMainForm.View_Standard_Toolbar_ActionExecute(Sender: TObject);
begin
  View_Standard_Toolbar_Action.Checked := not tb_Standard_Toolbar.Visible;
  tb_Standard_Toolbar.Visible := View_Standard_Toolbar_Action.Checked;
end;

procedure TMainForm.View_Debug_Toolbar_ActionExecute(Sender: TObject);
begin
  View_Debug_Toolbar_Action.Checked := not tb_Debug_Toolbar.Visible;
  tb_Debug_Toolbar.Visible := View_Debug_Toolbar_Action.Checked;
end;

procedure TMainForm.View_Build_Minibar_Toolbar_ActionExecute(
  Sender: TObject);
begin
  View_Build_Minibar_Toolbar_Action.Checked := not tb_Build_Minibar_Toolbar.Visible;
  tb_Build_Minibar_Toolbar.Visible := View_Build_Minibar_Toolbar_Action.Checked;
end;

procedure TMainForm.View_Edit_Toolbar_ActionExecute(Sender: TObject);
begin
  View_Edit_Toolbar_Action.Checked := not tb_Edit_Toolbar.Visible;
  tb_Edit_Toolbar.Visible := View_Edit_Toolbar_Action.Checked;
end;

procedure TMainForm.View_Variables_ActionExecute(Sender: TObject);
begin
  Variables_ToolButton.Down := not VariablesForm.Visible;
  View_Variables_Action.Checked := Variables_ToolButton.Down;
  if Variables_ToolButton.Down then
    ShowDockForm(VariablesForm)
  else VariablesForm.Close;
end;

procedure TMainForm.View_Debug_Windows_ActionExecute(Sender: TObject);
begin
//
end;

procedure TMainForm.View_CallStack_ActionExecute(Sender: TObject);
begin
  CallStack_ToolButton.Down := not CallStackForm.Visible;
  View_CallStack_Action.Checked := CallStack_ToolButton.Down;
  if CallStack_ToolButton.Down then
    ShowDockForm(CallStackForm)
  else CallStackForm.Close;
end;

procedure TMainForm.LoadDockInfo;
begin
  CreateDockableForm;
  GetToolbarWidthArr;
  {$IFDEF USEJVCL}
  JvAppStorage := TJvAppIniFileStorage.Create(self);
  JvAppStorage.Filename := ExtractFilePath(Application.ExeName) + 'DockInfo.ini';
  LoadDockTreeFromAppStorage(JvAppStorage);
  {$ELSE}
  LoadDockTreeFromFile(ExtractFilePath(Application.ExeName) + 'DockInfo.ini');
  {$ENDIF}
  SetToolButtonDownAndActionCheck;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LoadDockInfo;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  openfilename:String;
begin
  openfilename := ExtractFilePath(Application.ExeName)+'..\C++ File\MyApp.cpp';
  if FileExists( openfilename) then begin
      FindOrOpenFile(openfilename);
  end;
  View_Workspace_ActionExecute(Sender);
  if Assigned(WorkSpaceForm) then begin
     ShowDockForm(WorkSpaceForm);
     WorkSpaceForm.ClassView_TreeView.FullExpand;
     // --- HOW TO DOCK A WINDOW TO A DOCK SERVER IN CODE: ---
     WorkSpaceForm.ManualDock(lbDockServer1.LeftDockPanel );
  end;
end;

end.
