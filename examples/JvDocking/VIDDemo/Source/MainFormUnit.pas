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
unit MainFormUnit;
{$I jvcl.inc}
interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ImgList, ActnList, Menus, ToolWin, ComCtrls, ExtCtrls, StdCtrls,
  JvDockControlForm, JvDockVIDStyle
  {$IFDEF USEJVCL}
  , JvComponent, JvAppStorage, JvAppIniStorage
  {$ENDIF};

type
  TMainForm = class(TForm)
    ImageList1: TImageList;
    ActionList1: TActionList;
    Action_New_Project: TAction;
    Action_Open_Project: TAction;
    Action_Close_All: TAction;
    Action_Add_Project: TAction;
    Action_Remove_Project: TAction;
    Action_New_File: TAction;
    Action_Open_File: TAction;
    Action_Close: TAction;
    Action_Save_Selected_Items: TAction;
    Action_Save_Selected_Items_As: TAction;
    Action_Save_All: TAction;
    Action_Page_Setup: TAction;
    Action_Print: TAction;
    Action_Recent_File_List: TAction;
    Action_Exit: TAction;
    Action_Undo: TAction;
    Action_Open: TAction;
    Action_Add_Item: TAction;
    Action_Start: TAction;
    Action_Link: TAction;
    Action_Insert_Table: TAction;
    Action_Bold: TAction;
    Action_View_Links_on_WWW: TAction;
    Action_Split: TAction;
    Action_Contents: TAction;
    Action_Redo: TAction;
    Action_Cut: TAction;
    Action_Copy: TAction;
    Action_Paste: TAction;
    Action_Paste_As_HTML: TAction;
    Action_Delete: TAction;
    Action_Select_All: TAction;
    Action_Find_and_Replace: TAction;
    Action_Go_To: TAction;
    Action_List_Member: TAction;
    Action_Parameter_Info: TAction;
    Action_Complete_Word: TAction;
    Action_Insert_File_As_Text: TAction;
    Action_Open_With: TAction;
    Action_View_in_Browser: TAction;
    Action_Browse_With: TAction;
    Action_Sync_Script_Outline: TAction;
    Action_View_Controls_As_Text: TAction;
    Action_View_Controls_Graphically: TAction;
    Action_Project_Explorer: TAction;
    Action_Properties_Window: TAction;
    Action_Toolbox: TAction;
    Action_Immediate: TAction;
    Action_Autos: TAction;
    Action_Locals: TAction;
    Action_Watch: TAction;
    Action_Threads: TAction;
    Action_Call_Stack: TAction;
    Action_Running_Documents: TAction;
    Action_Task_List: TAction;
    Action_Visual_Component_Manager: TAction;
    Action_Object_Browser: TAction;
    Action_Document_Outline: TAction;
    Action_Output: TAction;
    Action_Data_View: TAction;
    Action_Script_Outline: TAction;
    Action_Previous_View: TAction;
    Action_All: TAction;
    Action_Comment: TAction;
    Action_Compile_Build_Deploy: TAction;
    Action_User: TAction;
    Action_Shortcut: TAction;
    Action_Smart_Editor: TAction;
    Action_Current_File: TAction;
    Action_Checked: TAction;
    Action_Unchecked: TAction;
    Action_Define_Window_Layout: TAction;
    Action_Full_Screen: TAction;
    Action_Property_Pages: TAction;
    Action_Refresh: TAction;
    Action_Start_Without_Debugging: TAction;
    Action_Break: TAction;
    Action_End: TAction;
    Action_Detach_All_Processes: TAction;
    Action_Restart: TAction;
    Action_Run_To_Cursor: TAction;
    Action_Step_Into: TAction;
    Action_Step_Over: TAction;
    Action_Step_Out: TAction;
    Action_Add_Watch: TAction;
    Action_Insert_Breakpoint: TAction;
    Action_Enable_Breakpoint: TAction;
    Action_Clear_All_Breakpoints: TAction;
    Action_Breakpoints: TAction;
    Action_Set_Next_Statement: TAction;
    Action_Show_Next_Statement: TAction;
    Action_Processes: TAction;
    Action_Bookmark: TAction;
    Action_Image: TAction;
    Action_Form: TAction;
    Action_Div: TAction;
    Action_Marquee: TAction;
    Action_Client: TAction;
    Action_Server: TAction;
    Action_HTML_Wizards: TAction;
    Action_Span: TAction;
    Action_Insert_Row: TAction;
    Action_Delete_Row: TAction;
    Action_Insert_Column: TAction;
    Action_Delete_Column: TAction;
    Action_Insert_Cell: TAction;
    Action_Delete_Cell: TAction;
    Action_Merge_Cells: TAction;
    Action_Split_Cell: TAction;
    Action_Italic: TAction;
    Action_Underline: TAction;
    Action_Superscript: TAction;
    Action_Subscript: TAction;
    Action_Decrease_Indent: TAction;
    Action_IncreaseIndent: TAction;
    Action_Absolute_Positioning: TAction;
    Action_Lock: TAction;
    Action_Customize_Toolbox: TAction;
    Action_Add_In_Manager: TAction;
    Action_Options: TAction;
    Action_Dockable: TAction;
    Action_Hide: TAction;
    Action_Cascade: TAction;
    Action_Tile_Horizontally: TAction;
    Action_Tile_Vertically: TAction;
    Action_Close_All_Documents: TAction;
    Action_Window_List: TAction;
    Action_Windows: TAction;
    Action_Index: TAction;
    Action_Search: TAction;
    Action_Technical_Support: TAction;
    Action_Microsoft_on_the_Web: TAction;
    Action_About: TAction;
    MainMenu1: TMainMenu;
    MainMenu_File: TMenuItem;
    Action_File: TAction;
    Action_Edit: TAction;
    Action_View: TAction;
    Action_Project: TAction;
    Action_Debug: TAction;
    Action_HTML: TAction;
    Action_Table: TAction;
    Action_Format: TAction;
    Action_Tools: TAction;
    Action_Window: TAction;
    Action_Help: TAction;
    NewProject1: TMenuItem;
    OpenProject1: TMenuItem;
    CloseAll1: TMenuItem;
    AddProject1: TMenuItem;
    N1: TMenuItem;
    RemoveProject1: TMenuItem;
    N2: TMenuItem;
    NewFile1: TMenuItem;
    OpenFile1: TMenuItem;
    N3: TMenuItem;
    SaveSelectedItems1: TMenuItem;
    SaveSelectedItemsAs1: TMenuItem;
    SaveAll1: TMenuItem;
    N4: TMenuItem;
    PageSetup1: TMenuItem;
    Print1: TMenuItem;
    N5: TMenuItem;
    RecentFileList1: TMenuItem;
    N6: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    Cut1: TMenuItem;
    N7: TMenuItem;
    Delete1: TMenuItem;
    Paste1: TMenuItem;
    PasteAsHTML1: TMenuItem;
    Delete2: TMenuItem;
    SelectAll1: TMenuItem;
    FindandReplace1: TMenuItem;
    GoTo1: TMenuItem;
    ListMembers1: TMenuItem;
    ParameterInfo1: TMenuItem;
    CompleteWord1: TMenuItem;
    InsertFileAsText1: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    N10: TMenuItem;
    View1: TMenuItem;
    Open1: TMenuItem;
    OpenWith1: TMenuItem;
    ViewinBrowser1: TMenuItem;
    BrowseWith1: TMenuItem;
    SyncScriptOutline1: TMenuItem;
    ViewControlsAsText1: TMenuItem;
    ViewControlsGraphically1: TMenuItem;
    ProjectExplorer1: TMenuItem;
    PropertiesWindow1: TMenuItem;
    Toolbox1: TMenuItem;
    DefineWindowLayout1: TMenuItem;
    FullScreen1: TMenuItem;
    Refresh1: TMenuItem;
    N11: TMenuItem;
    N12: TMenuItem;
    N13: TMenuItem;
    Debug_Windows1: TMenuItem;
    Immediate1: TMenuItem;
    Autos1: TMenuItem;
    Locals1: TMenuItem;
    Watch1: TMenuItem;
    Threads1: TMenuItem;
    CallStack1: TMenuItem;
    RunningDocuments1: TMenuItem;
    Other_Windows1: TMenuItem;
    TaskList1: TMenuItem;
    VisualComponentManager1: TMenuItem;
    ObjectBrowser1: TMenuItem;
    DocumentOutline1: TMenuItem;
    Output1: TMenuItem;
    DataView1: TMenuItem;
    ScriptOutline1: TMenuItem;
    Show_Tasks1: TMenuItem;
    N14: TMenuItem;
    Toolbars1: TMenuItem;
    PreviousView1: TMenuItem;
    All1: TMenuItem;
    Comment1: TMenuItem;
    CompileBuildDeploy1: TMenuItem;
    User1: TMenuItem;
    Shortcut1: TMenuItem;
    SmartEditor1: TMenuItem;
    CurrentFile1: TMenuItem;
    Checked1: TMenuItem;
    Unchecked1: TMenuItem;
    Action_Toolbars_Standard: TAction;
    Action_Toolbars_Debug: TAction;
    Standard1: TMenuItem;
    Debug1: TMenuItem;
    N15: TMenuItem;
    Action_Toolbars_Customize: TAction;
    Customize1: TMenuItem;
    PropertyPages1: TMenuItem;
    N16: TMenuItem;
    N17: TMenuItem;
    Project1: TMenuItem;
    AddItem1: TMenuItem;
    Debug2: TMenuItem;
    Start1: TMenuItem;
    StartWithoutDebugging1: TMenuItem;
    Break1: TMenuItem;
    End1: TMenuItem;
    DetachAllProcesses1: TMenuItem;
    Restart1: TMenuItem;
    RunToCursor1: TMenuItem;
    StepInto1: TMenuItem;
    StepOver1: TMenuItem;
    StepOut1: TMenuItem;
    AddWatch1: TMenuItem;
    InsertBreakpoint1: TMenuItem;
    EnableBreakpoint1: TMenuItem;
    ClearAllBreakpoints1: TMenuItem;
    Breakpoints1: TMenuItem;
    SetNextStatement1: TMenuItem;
    ShowNextStatement1: TMenuItem;
    Processes1: TMenuItem;
    N18: TMenuItem;
    N19: TMenuItem;
    N20: TMenuItem;
    N21: TMenuItem;
    N22: TMenuItem;
    HTML1: TMenuItem;
    Link1: TMenuItem;
    Bookmark1: TMenuItem;
    Image1: TMenuItem;
    Form1: TMenuItem;
    Div1: TMenuItem;
    Span1: TMenuItem;
    Marquee1: TMenuItem;
    HTMLWizards1: TMenuItem;
    N23: TMenuItem;
    Action_HTML_Script_Block: TAction;
    ScriptBlock1: TMenuItem;
    Client1: TMenuItem;
    Server1: TMenuItem;
    Table1: TMenuItem;
    InsertTable1: TMenuItem;
    InsertRow1: TMenuItem;
    DeleteRow1: TMenuItem;
    InsertColumn1: TMenuItem;
    DeleteColumn1: TMenuItem;
    InsertCell1: TMenuItem;
    DeleteCell1: TMenuItem;
    MergeCells1: TMenuItem;
    SplitCell1: TMenuItem;
    N24: TMenuItem;
    N25: TMenuItem;
    N26: TMenuItem;
    Format1: TMenuItem;
    Bold1: TMenuItem;
    Italic1: TMenuItem;
    Underline1: TMenuItem;
    Superscript1: TMenuItem;
    Subscript1: TMenuItem;
    DecreaseIndent1: TMenuItem;
    IncreaseIndent1: TMenuItem;
    AbsolutePositioning1: TMenuItem;
    Lock1: TMenuItem;
    N27: TMenuItem;
    N28: TMenuItem;
    N29: TMenuItem;
    Tools1: TMenuItem;
    ViewLinksonWWW1: TMenuItem;
    CustomizeToolbox1: TMenuItem;
    AddInManager1: TMenuItem;
    Options1: TMenuItem;
    N30: TMenuItem;
    Window1: TMenuItem;
    Split1: TMenuItem;
    Dockable1: TMenuItem;
    Hide1: TMenuItem;
    Cascade1: TMenuItem;
    TileHorizontally1: TMenuItem;
    TileVertically1: TMenuItem;
    CloseAllDocuments1: TMenuItem;
    WindowList1: TMenuItem;
    Windows1: TMenuItem;
    N31: TMenuItem;
    N32: TMenuItem;
    N33: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    Index1: TMenuItem;
    Search1: TMenuItem;
    TechnicalSupport1: TMenuItem;
    MicrosoftontheWeb1: TMenuItem;
    About1: TMenuItem;
    N34: TMenuItem;
    N35: TMenuItem;
    Action_Free_Stuff: TAction;
    Action_Product_News: TAction;
    Action_Frequently_Asked_Questions: TAction;
    Action_Send_Feedback: TAction;
    Action_Online_Support: TAction;
    Action_Microsoft_Developer_Network_Online: TAction;
    Action_Best_of_the_Web: TAction;
    Action_Search_the_Web: TAction;
    Action_Web_Tutorial: TAction;
    Action_Microsoft_Home_Page: TAction;
    FreeStuff1: TMenuItem;
    ProductNews1: TMenuItem;
    FrequentlyAskedQuestions1: TMenuItem;
    OnlineSupport1: TMenuItem;
    MicrosoftDeveloperNetworkOnline1: TMenuItem;
    SendFeedback1: TMenuItem;
    BestoftheWeb1: TMenuItem;
    SearchtheWeb1: TMenuItem;
    WebTutorial1: TMenuItem;
    MicrosoftHomePage1: TMenuItem;
    N36: TMenuItem;
    N37: TMenuItem;
    MainControlBar: TControlBar;
    ToolBar_Build: TToolBar;
    ToolButton_Build_Project: TToolButton;
    ToolButton_Rebuild_Solution: TToolButton;
    ToolButton_Cancel: TToolButton;
    ToolBar_Standard: TToolBar;
    ToolButton_New_Project: TToolButton;
    ToolButton_Add_Item: TToolButton;
    ToolButton_Open_Project: TToolButton;
    ToolButton_Save_Selected_Items: TToolButton;
    ToolButton_Save_All: TToolButton;
    ToolButton_Cut: TToolButton;
    ToolButton_Copy: TToolButton;
    ToolButton_Paste: TToolButton;
    ToolButton_Undo: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton_Redo: TToolButton;
    ToolButton_Start1: TToolButton;
    ToolButton_Break1: TToolButton;
    ToolButton19: TToolButton;
    ToolButton_End1: TToolButton;
    ToolButton_Find_and_Replace: TToolButton;
    Toolbar_Find_ComboBox: TComboBox;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton_Project_Explorer: TToolButton;
    ToolButton_Properties_Window: TToolButton;
    ToolButton_Toolbox: TToolButton;
    ToolButton_Task_List: TToolButton;
    ToolButton30: TToolButton;
    Load_Save_WindowUI_ComboBox: TComboBox;
    Other_Windows_PopupMenu: TPopupMenu;
    TaskList2: TMenuItem;
    VisualComponentManager2: TMenuItem;
    ObjectBrowser2: TMenuItem;
    DocumentOutline2: TMenuItem;
    Output2: TMenuItem;
    DataView2: TMenuItem;
    ScriptOutline2: TMenuItem;
    ToolBar_Debug: TToolBar;
    ToolButton_Start2: TToolButton;
    ToolButton_Start_Without_Debugging: TToolButton;
    ToolButton_Break2: TToolButton;
    ToolButton_End2: TToolButton;
    ToolButton_Detach_All_Processes: TToolButton;
    ToolButton_Restart: TToolButton;
    ToolButton_Run_To_Cursor: TToolButton;
    ToolButton_Step_Into: TToolButton;
    ToolButton36: TToolButton;
    ToolButton_Step_Over: TToolButton;
    ToolButton_Step_Out: TToolButton;
    ToolButton39: TToolButton;
    ToolButton_Insert_Breakpoint: TToolButton;
    ToolButton_Enable_Breakpoint: TToolButton;
    ToolButton_Clear_All_Breakpoints: TToolButton;
    ToolButton_Breakpoints: TToolButton;
    ToolButton44: TToolButton;
    ToolButton_Immediate: TToolButton;
    ToolButton_Autos: TToolButton;
    ToolButton_Locals: TToolButton;
    ToolButton_Watch: TToolButton;
    ToolButton_Threads: TToolButton;
    ToolButton_Call_Stack: TToolButton;
    ToolButton_Running_Documents: TToolButton;
    ToolButton_Output: TToolButton;
    ToolButton53: TToolButton;
    ToolButton_Processes: TToolButton;
    Action_Build_Project: TAction;
    Action_Build_Solution: TAction;
    Action_Rebuild_Solution: TAction;
    Action_Cancel: TAction;
    Action_Build: TAction;
    ActionBuild1: TMenuItem;
    BuildProject1: TMenuItem;
    BuildSolution1: TMenuItem;
    RebuildSolution1: TMenuItem;
    Cancel1: TMenuItem;
    ComboBox_Configuration: TComboBox;
    Action_Toolbars_Build: TAction;
    Build1: TMenuItem;
    lbDockServer1: TJvDockServer;
    JvDockVIDStyle1: TJvDockVIDStyle;
    MainFormStatusBar: TStatusBar;
    procedure Action_FileExecute(Sender: TObject);
    procedure MainControlBarBandMove(Sender: TObject; Control: TControl;
      var ARect: TRect);
    procedure Action_New_ProjectExecute(Sender: TObject);
    procedure Action_ExitExecute(Sender: TObject);
    procedure Action_Task_ListExecute(Sender: TObject);
    procedure MainFormStatusBarResize(Sender: TObject);
    procedure Action_Project_ExplorerExecute(Sender: TObject);
    procedure Action_Properties_WindowExecute(Sender: TObject);
    procedure Action_ToolboxExecute(Sender: TObject);
    procedure Action_ImmediateExecute(Sender: TObject);
    procedure Action_AutosExecute(Sender: TObject);
    procedure Action_LocalsExecute(Sender: TObject);
    procedure Action_WatchExecute(Sender: TObject);
    procedure Action_ThreadsExecute(Sender: TObject);
    procedure Action_Call_StackExecute(Sender: TObject);
    procedure Action_Running_DocumentsExecute(Sender: TObject);
    procedure Action_Document_OutlineExecute(Sender: TObject);
    procedure Action_OutputExecute(Sender: TObject);
    procedure Action_Script_OutlineExecute(Sender: TObject);
    procedure Action_Define_Window_LayoutExecute(Sender: TObject);
    procedure Action_Find_and_ReplaceExecute(Sender: TObject);
    procedure Load_Save_WindowUI_ComboBoxChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Action_AboutExecute(Sender: TObject);
  private
    { Private declarations }
    FCreated: Boolean;  
    procedure CreateDockableForm;
    procedure GetToolbarWidthArr;
    procedure SaveDefaultLayout;
    procedure LoadDefaultLayout;
  public
    { Public declarations }
    {$IFDEF USEJVCL}
    JvAppStorage:TJvAppIniFileStorage;
    {$ENDIF}
    procedure LoadDockInfo;
  end;

var
  MainForm: TMainForm;

const DefineWindowLayoutFileName: string = 'DefineWindowLayout.ini';
const SectionString: string = 'DefineWindowLayout';
const DefaultLayout: string = 'DefaultLayout';

implementation

uses IniFiles, ProjectExplorerUnit, PropertiesUnit, ToolboxUnit, ImmediateUnit,
  AutosUnit, LocalsUnit, WatchUnit, ThreadsUnit, CallStackUnit,
  RunningDocumentsUnit, TaskListUnit, DocumentOutlineUnit, OutputUnit,
  ScriptOutlineUnit, DefineWindowLayoutUnit, FindAndReplaceUnit, SplashUnit;

procedure TMainForm.CreateDockableForm;
begin
  ProjectExplorerForm := TProjectExplorerForm.Create(Self);   //创建Project Explorer工具窗体
  PropertiesForm := TPropertiesForm.Create(Self);             //创建Properties工具窗体
  ToolboxForm := TToolboxForm.Create(Self);                   //创建Toolbox工具窗体
  {----------------------------------------------------------------------------}
  ImmediateForm := TImmediateForm.Create(Self);               //创建Immediate工具窗体
  AutosForm := TAutosForm.Create(Self);                       //创建Autos工具窗体
  LocalsForm := TLocalsForm.Create(Self);                     //创建Locals工具窗体
  WatchForm := TWatchForm.Create(Self);                       //创建Watch工具窗体
  ThreadsForm := TThreadsForm.Create(Self);                   //创建Threads工具窗体
  CallStackForm := TCallStackForm.Create(Self);               //创建Call Stack工具窗体
  RunningDocumentsForm := TRunningDocumentsForm.Create(Self); //创建Running Documents工具窗体
  {----------------------------------------------------------------------------}
  TaskListForm := TTaskListForm.Create(Self);                 //创建Task List工具窗体
  DocumentOutlineForm := TDocumentOutlineForm.Create(Self);   //创建Document Outline工具窗体
  OutputForm := TOutputForm.Create(Self);                     //创建Output工具窗体
  ScriptOutlineForm := TScriptOutlineForm.Create(Self);       //创建Script Outline工具窗体
  {----------------------------------------------------------------------------}
  FindAndReplaceForm := TFindAndReplaceForm.Create(Self);     //创建Find And Replace工具窗体
end;

{$R *.DFM}

procedure TMainForm.Action_FileExecute(Sender: TObject);
begin
 //
end;

procedure TMainForm.GetToolbarWidthArr;
var i: Integer;
begin
  //
  for i := 0 to MainControlBar.ControlCount - 1 do
    MainControlBar.Controls[i].Tag := MainControlBar.Controls[i].Width;
  FCreated := True;
end;

procedure TMainForm.MainControlBarBandMove(Sender: TObject;
  Control: TControl; var ARect: TRect);
begin
  //
  if (Control.Tag <> ARect.Right - ARect.Left - 13) and FCreated then
  begin
    ARect.Right := ARect.Left + Control.Tag + 13;
  end;
end;

procedure TMainForm.Action_New_ProjectExecute(Sender: TObject);
begin
  MainFormStatusBar.Panels[2].Text := Format('You clicked ''%s''', [TAction(Sender).Caption]);
end;

procedure TMainForm.Action_ExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.MainFormStatusBarResize(Sender: TObject);
begin
  if MainFormStatusBar.Width > 450 then
    MainFormStatusBar.Panels[0].Width := MainFormStatusBar.Width - 350
  else
    MainFormStatusBar.Panels[2].Width := MainFormStatusBar.Width - 200;
end;

procedure TMainForm.Action_Task_ListExecute(Sender: TObject);
begin
  { 显示Task List工具窗体 }
  ShowDockForm(TaskListForm);
end;

procedure TMainForm.Action_Project_ExplorerExecute(Sender: TObject);
begin
  { 显示Task List工具窗体 }
  ShowDockForm(ProjectExplorerForm);
end;

procedure TMainForm.Action_Properties_WindowExecute(Sender: TObject);
begin
  { 显示Properties工具窗体 }
  ShowDockForm(PropertiesForm);
end;

procedure TMainForm.Action_ToolboxExecute(Sender: TObject);
begin
  { 显示Toolbox工具窗体 }
  ShowDockForm(ToolboxForm);
end;

procedure TMainForm.Action_ImmediateExecute(Sender: TObject);
begin
  { 显示Immediate工具窗体 }
  ShowDockForm(ImmediateForm);
end;

procedure TMainForm.Action_AutosExecute(Sender: TObject);
begin
  { 显示Autos工具窗体 }
  ShowDockForm(AutosForm);
end;

procedure TMainForm.Action_LocalsExecute(Sender: TObject);
begin
  { 显示Locals工具窗体 }
  ShowDockForm(LocalsForm);
end;

procedure TMainForm.Action_WatchExecute(Sender: TObject);
begin
  { 显示Watch工具窗体 }
  ShowDockForm(WatchForm);
end;

procedure TMainForm.Action_ThreadsExecute(Sender: TObject);
begin
  { 显示Threads工具窗体 }
  ShowDockForm(ThreadsForm);
end;

procedure TMainForm.Action_Call_StackExecute(Sender: TObject);
begin
  { 显示Call Stack工具窗体 }
  ShowDockForm(CallStackForm);
end;

procedure TMainForm.Action_Running_DocumentsExecute(Sender: TObject);
begin
  { 显示Running Documents工具窗体 }
  ShowDockForm(RunningDocumentsForm);
end;

procedure TMainForm.Action_Document_OutlineExecute(Sender: TObject);
begin
  { 显示Document Outline工具窗体 }
  ShowDockForm(DocumentOutlineForm);
end;

procedure TMainForm.Action_OutputExecute(Sender: TObject);
begin
  { 显示Output工具窗体 }
  ShowDockForm(OutputForm);
end;

procedure TMainForm.Action_Script_OutlineExecute(Sender: TObject);
begin
  { 显示Script Outline工具窗体 }
  ShowDockForm(ScriptOutlineForm);
end;

procedure TMainForm.Action_Find_and_ReplaceExecute(Sender: TObject);
begin
  { 显示Find And Replace工具窗体 }
  ShowDockForm(FindAndReplaceForm);
end;

procedure TMainForm.Action_Define_Window_LayoutExecute(Sender: TObject);
begin
  with TDefineWindowLayoutForm.Create(nil) do
  begin
    ShowModal;
    Load_Save_WindowUI_ComboBox.Items.Clear;
    Load_Save_WindowUI_ComboBox.Items.Assign(Views_ListBox.Items);
    Load_Save_WindowUI_ComboBox.ItemIndex := SelectItemIndex;
    Release;
  end;
end;

procedure TMainForm.LoadDockInfo;
begin
  CreateDockableForm;
  GetToolbarWidthArr;
  LoadDefaultLayout;
end;

procedure TMainForm.LoadDefaultLayout;
var IniFile: TIniFile;
  Str: string;
  Sections: TStringList;
  Index: Integer;
begin
  IniFile := TIniFile.Create(ExtractFilePath(Application.EXEName) + DefineWindowLayoutFileName);
  Sections := TStringList.Create;
  try
    IniFile.ReadSection(SectionString, Sections);
    Index := Sections.IndexOf(DefaultLayout);
    if Index <> - 1 then
      Sections.Delete(Sections.IndexOf(DefaultLayout));
    Sections.Sort;
    Load_Save_WindowUI_ComboBox.Items.Assign(Sections);
    Load_Save_WindowUI_ComboBox.ItemIndex := 0;
    Str := IniFile.ReadString(SectionString, DefaultLayout, 'ERROR');
    if Str <> 'ERROR' then
    begin
      Load_Save_WindowUI_ComboBox.ItemIndex := Load_Save_WindowUI_ComboBox.Items.IndexOf(Str);
      {$IFDEF USEJVCL}
      JvAppStorage.Filename := ExtractFilePath(Application.ExeName) + Str + '.ini';
      LoadDockTreeFromAppStorage(JvAppStorage);
      {$ELSE}
      LoadDockTreeFromFile(ExtractFilePath(Application.EXEName) + Str + '.ini');
      {$ENDIF}
    end;
  finally
    Sections.Free;
    IniFile.Free;
  end;
end;

procedure TMainForm.SaveDefaultLayout;
//var IniFile: TIniFile;
begin
  {$IFDEF USEJVCL}
  JvAppStorage.Filename := ExtractFilePath(Application.EXEName) + Load_Save_WindowUI_ComboBox.Text + '.ini';
  SaveDockTreeToAppStorage(JvAppStorage, Load_Save_WindowUI_ComboBox.Text);
{  JvAppStorage.WriteString(JvAppStorage.ConcatPaths([SectionString, DefaultLayout]), Load_Save_WindowUI_ComboBox.Text);
  IniFile := TIniFile.Create(ExtractFilePath(Application.EXEName) + DefineWindowLayoutFileName);
  try
    IniFile.WriteString(SectionString, DefaultLayout, Load_Save_WindowUI_ComboBox.Text);
  finally
    IniFile.Free;
  end;}
  {$ELSE}
    SaveDockTreeToFile(ExtractFilePath(Application.EXEName) + Load_Save_WindowUI_ComboBox.Text + '.ini');
  {$ENDIF}
end;

procedure TMainForm.Load_Save_WindowUI_ComboBoxChange(Sender: TObject);
begin
  {$IFNDEF USEJVCL}
  LoadDockTreeFromFile(ExtractFilePath(Application.EXEName) + Load_Save_WindowUI_ComboBox.Text + '.ini');
  {$ENDIF}
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  { 保存停靠信息的代码不能放在OnDestroy事件中，因为这时候有些窗口已经被释放了，不能被保存。 }
  SaveDefaultLayout;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
(*  ProjectExplorerForm.Release;
  PropertiesForm.Release;
  ToolboxForm.Release;
  {----------------------------------------------------------------------------}
  ImmediateForm.Release;
  AutosForm.Release;
  LocalsForm.Release;
  WatchForm.Release;
  ThreadsForm.Release;
  CallStackForm.Release;
  RunningDocumentsForm.Release;
  {----------------------------------------------------------------------------}
  TaskListForm.Release;
  DocumentOutlineForm.Release;
  OutputForm.Release;
  ScriptOutlineForm.Release;
  {----------------------------------------------------------------------------}
  FindAndReplaceForm.Release;*)
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  {$IFDEF USEJVCL}
  JvAppStorage := TJvAppIniFileStorage.Create(self);
  {$ENDIF}
  LoadDockInfo;
end;

procedure TMainForm.Action_AboutExecute(Sender: TObject);
begin
 with TSplashForm.Create(nil) do
 try
   ShowModal;
 finally
   Free;
 end;
end;

end.
