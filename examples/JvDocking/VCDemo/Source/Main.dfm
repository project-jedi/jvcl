object MainForm: TMainForm
  Left = 262
  Top = 156
  Width = 648
  Height = 508
  Caption = 'Macrosoft Visual C++'
  Color = clGray
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  FormStyle = fsMDIForm
  OldCreateOrder = False
  PopupMenu = ViewPopupMenu
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainControlBar: TControlBar
    Left = 0
    Top = 0
    Width = 640
    Height = 104
    Align = alTop
    AutoSize = True
    BevelEdges = []
    Color = clBtnFace
    ParentColor = False
    PopupMenu = ViewPopupMenu
    TabOrder = 0
    OnBandMove = MainControlBarBandMove
    OnResize = MainControlBarResize
    DesignSize = (
      640
      104)
    object tb_Standard_ToolBar: TToolBar
      Tag = 1
      Left = 11
      Top = 28
      Width = 545
      Height = 22
      Align = alClient
      Anchors = []
      AutoSize = True
      Caption = 'Standard'
      EdgeBorders = []
      Flat = True
      Images = Action_ImageList
      TabOrder = 0
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = File_New_Action
        Grouped = True
      end
      object ToolButton2: TToolButton
        Left = 23
        Top = 0
        Action = File_Open_Action
        Grouped = True
      end
      object ToolButton3: TToolButton
        Left = 46
        Top = 0
        Action = File_Save_Action
        Grouped = True
      end
      object ToolButton4: TToolButton
        Left = 69
        Top = 0
        Action = File_SaveAll_Action
        Grouped = True
      end
      object ToolButton10: TToolButton
        Left = 92
        Top = 0
        Width = 8
        Caption = 'ToolButton10'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object ToolButton5: TToolButton
        Left = 100
        Top = 0
        Action = Edit_Cut_Action
        Grouped = True
      end
      object ToolButton6: TToolButton
        Left = 123
        Top = 0
        Action = Edit_Copy_Action
        Grouped = True
      end
      object ToolButton7: TToolButton
        Left = 146
        Top = 0
        Action = Edit_Paste_Action
        Grouped = True
      end
      object ToolButton11: TToolButton
        Left = 169
        Top = 0
        Width = 8
        Caption = 'ToolButton11'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 177
        Top = 0
        Action = Edit_Undo_Action
        Grouped = True
      end
      object ToolButton9: TToolButton
        Left = 200
        Top = 0
        Action = Edit_Redo_Action
        Grouped = True
      end
      object ToolButton12: TToolButton
        Left = 223
        Top = 0
        Width = 8
        Caption = 'ToolButton12'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object WorkSpace_ToolButton: TToolButton
        Left = 231
        Top = 0
        Action = View_Workspace_Action
        Grouped = True
      end
      object Output_ToolButton: TToolButton
        Left = 254
        Top = 0
        Action = View_OutPut_Action
        Grouped = True
      end
      object ToolButton15: TToolButton
        Left = 277
        Top = 0
        Action = Window_Windows_Action
        Grouped = True
      end
      object ToolButton16: TToolButton
        Left = 300
        Top = 0
        Action = Edit_Find_In_File_Action
        Grouped = True
      end
      object Search_Panel: TPanel
        Left = 323
        Top = 0
        Width = 190
        Height = 22
        BevelOuter = bvNone
        TabOrder = 0
        object ComboBox1: TComboBox
          Left = 1
          Top = 1
          Width = 190
          Height = 21
          ImeName = #215#207#185#226#198#180#210#244#202#228#200#235#183#168
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object ToolButton17: TToolButton
        Left = 513
        Top = 0
        Width = 8
        Caption = 'ToolButton17'
        ImageIndex = 13
        Style = tbsSeparator
      end
      object ToolButton18: TToolButton
        Left = 521
        Top = 0
        Action = Help_Search_Action
        Grouped = True
      end
    end
    object tb_Edit_ToolBar: TToolBar
      Tag = 2
      Left = 11
      Top = 80
      Width = 206
      Height = 22
      Align = alClient
      AutoSize = True
      Caption = 'Edit'
      EdgeBorders = []
      Flat = True
      Images = Action_ImageList
      TabOrder = 1
      object ToolButton19: TToolButton
        Left = 0
        Top = 0
        Action = Edit_Toggle_Bookmark_Action
      end
      object ToolButton20: TToolButton
        Left = 23
        Top = 0
        Action = Edit_Next_Bookmark_Action
      end
      object ToolButton21: TToolButton
        Left = 46
        Top = 0
        Action = Edit_Previous_Bookmark_Action
      end
      object ToolButton22: TToolButton
        Left = 69
        Top = 0
        Action = Edit_Clear_All_BookMarks_Action
      end
      object ToolButton27: TToolButton
        Left = 92
        Top = 0
        Width = 8
        Caption = 'ToolButton27'
        ImageIndex = 8
        Style = tbsSeparator
      end
      object ToolButton23: TToolButton
        Left = 100
        Top = 0
        Action = Edit_Find_Action
      end
      object ToolButton28: TToolButton
        Left = 123
        Top = 0
        Width = 8
        Caption = 'ToolButton28'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object ToolButton24: TToolButton
        Left = 131
        Top = 0
        Action = Edit_Increase_Indent_Action
      end
      object ToolButton25: TToolButton
        Left = 154
        Top = 0
        Action = Edit_Decrease_Indent_Action
      end
      object ToolButton26: TToolButton
        Left = 177
        Top = 0
        Action = Edit_Toggle_WhiteSpace_Display_Action
      end
    end
    object tb_Build_MiniBar_ToolBar: TToolBar
      Tag = 3
      Left = 417
      Top = 54
      Width = 139
      Height = 22
      Align = alClient
      AutoSize = True
      Caption = 'Edit'
      EdgeBorders = []
      Flat = True
      Images = Action_ImageList
      TabOrder = 2
      object ToolButton29: TToolButton
        Left = 0
        Top = 0
        Action = Build_Compile_Action
      end
      object ToolButton30: TToolButton
        Left = 23
        Top = 0
        Action = Build_Build_Action
      end
      object ToolButton31: TToolButton
        Left = 46
        Top = 0
        Action = Build_Stop_Build_Action
      end
      object ToolButton32: TToolButton
        Left = 69
        Top = 0
        Action = Build_Execute_Action
      end
      object ToolButton34: TToolButton
        Left = 92
        Top = 0
        Action = Debug_Go_Action
      end
      object ToolButton36: TToolButton
        Left = 115
        Top = 0
        Action = Build_Insert_Remove_Breakpoint_Action
      end
    end
    object tb_Debug_ToolBar: TToolBar
      Tag = 4
      Left = 11
      Top = 54
      Width = 393
      Height = 22
      Align = alClient
      AutoSize = True
      Caption = 'Edit'
      EdgeBorders = []
      Flat = True
      Images = Action_ImageList
      TabOrder = 3
      object ToolButton37: TToolButton
        Left = 0
        Top = 0
        Action = Debug_Restart_Action
      end
      object ToolButton38: TToolButton
        Left = 23
        Top = 0
        Action = Debug_Stop_Debugging_Action
      end
      object ToolButton39: TToolButton
        Left = 46
        Top = 0
        Action = Debug_Break_Action
      end
      object ToolButton40: TToolButton
        Left = 69
        Top = 0
        Action = Debug_Apply_Code_Changes_Action
      end
      object ToolButton41: TToolButton
        Left = 92
        Top = 0
        Width = 8
        Caption = 'ToolButton27'
        ImageIndex = 8
        Style = tbsSeparator
      end
      object ToolButton42: TToolButton
        Left = 100
        Top = 0
        Action = Debug_Show_Next_Statement_Action
      end
      object ToolButton44: TToolButton
        Left = 123
        Top = 0
        Action = Debug_Step_Into_Action
      end
      object ToolButton45: TToolButton
        Left = 146
        Top = 0
        Action = Debug_Step_Over_Action
      end
      object ToolButton46: TToolButton
        Left = 169
        Top = 0
        Action = Debug_Step_Out_Action
      end
      object ToolButton47: TToolButton
        Left = 192
        Top = 0
        Action = Debug_Run_to_Cursor_Action
      end
      object ToolButton43: TToolButton
        Left = 215
        Top = 0
        Width = 8
        Caption = 'ToolButton28'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object ToolButton48: TToolButton
        Left = 223
        Top = 0
        Action = Debug_QuickWatch_Action
      end
      object ToolButton55: TToolButton
        Left = 246
        Top = 0
        Width = 8
        Caption = 'ToolButton55'
        ImageIndex = 16
        Style = tbsSeparator
      end
      object Watch_ToolButton: TToolButton
        Left = 254
        Top = 0
        Action = View_Watch_Action
      end
      object Variables_ToolButton: TToolButton
        Left = 277
        Top = 0
        Action = View_Variables_Action
      end
      object Registers_ToolButton: TToolButton
        Left = 300
        Top = 0
        Action = View_Registers_Action
      end
      object Memory_ToolButton: TToolButton
        Left = 323
        Top = 0
        Action = View_Memory_Action
      end
      object CallStack_ToolButton: TToolButton
        Left = 346
        Top = 0
        Action = View_CallStack_Action
      end
      object ToolButton54: TToolButton
        Left = 369
        Top = 0
        Action = View_Disassembly_Action
      end
    end
    object MainMenu_ToolBar: TToolBar
      Left = 11
      Top = 2
      Width = 545
      Height = 22
      Align = alNone
      Anchors = [akLeft, akTop, akRight, akBottom]
      ButtonHeight = 19
      ButtonWidth = 57
      Caption = 'MainMenu_ToolBar'
      EdgeBorders = []
      EdgeInner = esNone
      EdgeOuter = esNone
      Flat = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      List = True
      ParentFont = False
      ShowCaptions = True
      TabOrder = 4
      OnCustomDraw = MainMenu_ToolBarCustomDraw
      OnMouseDown = MainMenu_ToolBarMouseDown
      OnMouseMove = MainMenu_ToolBarMouseMove
      OnMouseUp = MainMenu_ToolBarMouseUp
      object ToolButton13: TToolButton
        Left = 0
        Top = 0
        AutoSize = True
        Caption = '&File'
        Grouped = True
        MenuItem = File1
      end
      object ToolButton14: TToolButton
        Left = 27
        Top = 0
        AutoSize = True
        Caption = '&Edit'
        Grouped = True
        MenuItem = Edit1
      end
      object ToolButton33: TToolButton
        Left = 56
        Top = 0
        AutoSize = True
        Caption = '&View'
        Grouped = True
        MenuItem = View1
      end
      object ToolButton35: TToolButton
        Left = 90
        Top = 0
        AutoSize = True
        Caption = '&Insert'
        Grouped = True
        MenuItem = Insert1
      end
      object ToolButton49: TToolButton
        Left = 127
        Top = 0
        AutoSize = True
        Caption = '&Project'
        Grouped = True
        MenuItem = Project1
      end
      object ToolButton50: TToolButton
        Left = 171
        Top = 0
        AutoSize = True
        Caption = '&Build'
        Grouped = True
        MenuItem = Build1
      end
      object ToolButton51: TToolButton
        Left = 205
        Top = 0
        AutoSize = True
        Caption = '&Debug'
        Grouped = True
        MenuItem = Debug1
      end
      object ToolButton52: TToolButton
        Left = 248
        Top = 0
        AutoSize = True
        Caption = '&Window'
        Grouped = True
        MenuItem = Window1
      end
      object ToolButton53: TToolButton
        Left = 298
        Top = 0
        AutoSize = True
        Caption = '&Help'
        Grouped = True
        MenuItem = Help1
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 462
    Width = 640
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 50
      end>
  end
  object MainMenu1: TMainMenu
    Images = Action_ImageList
    Left = 40
    Top = 104
    object File1: TMenuItem
      Caption = '&File'
      object New1: TMenuItem
        Action = File_New_Action
      end
      object Open1: TMenuItem
        Action = File_Open_Action
      end
      object Open2: TMenuItem
        Action = File_Close_Action
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object OpenWorkSpace1: TMenuItem
        Action = File_Open_WorkSpace_Action
      end
      object SaveWorkSpace1: TMenuItem
        Action = File_Save_WorKSpace_Action
      end
      object CloseWorkSpace1: TMenuItem
        Action = File_Close_WorKSpace_Action
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Action = File_Save_Action
      end
      object SaveAs1: TMenuItem
        Action = File_SaveAs_Action
      end
      object SaveAll1: TMenuItem
        Action = File_SaveAll_Action
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object PageSetup1: TMenuItem
        Action = File_Page_Setup_Action
      end
      object Print1: TMenuItem
        Action = File_Print_Action
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object RecentFiles1: TMenuItem
        Action = File_Recent_Files_Action
      end
      object RecentWorkSpace1: TMenuItem
        Action = File_Recent_WorkSpace_Action
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = File_Exit_Action
      end
    end
    object Edit1: TMenuItem
      Caption = '&Edit'
      object Redo1: TMenuItem
        Action = Edit_Undo_Action
      end
      object Redo2: TMenuItem
        Action = Edit_Redo_Action
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Action = Edit_Cut_Action
      end
      object Cut2: TMenuItem
        Action = Edit_Copy_Action
      end
      object Paste1: TMenuItem
        Action = Edit_Paste_Action
      end
      object Delete1: TMenuItem
        Action = Edit_Delete_Action
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object SelectAll1: TMenuItem
        Action = Edit_SelectAll_Action
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Find1: TMenuItem
        Action = Edit_Find_Action
      end
      object FindInFile1: TMenuItem
        Action = Edit_Find_In_File_Action
      end
      object Replace1: TMenuItem
        Action = Edit_Replace_Action
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object GoTo2: TMenuItem
        Action = Edit_Goto_Action
      end
      object GoTo1: TMenuItem
        Action = Edit_Bookmarks_Action
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object Advanced1: TMenuItem
        Action = Edit_Advanced_Action
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object Breakpoints1: TMenuItem
        Action = Edit_Breakpoints_Action
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object ListMembers1: TMenuItem
        Action = Edit_List_Members_Action
      end
      object ypeInfo1: TMenuItem
        Action = Edit_Type_Info_Action
      end
      object ParameterInfo1: TMenuItem
        Action = Edit_Parameter_Info_Action
      end
      object CompleteWord1: TMenuItem
        Action = Edit_Complete_Word_Action
      end
    end
    object View1: TMenuItem
      Caption = '&View'
      object ResourceSymbols1: TMenuItem
        Action = View_Resource_Symbols_Action
      end
      object ResourceInclude1: TMenuItem
        Action = View_Resource_Include_Action
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object FullScreen1: TMenuItem
        Action = View_Full_Screen_Action
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object Workspace1: TMenuItem
        Action = View_Workspace_Action
      end
      object OutPut1: TMenuItem
        Action = View_OutPut_Action
      end
      object DebugWindows1: TMenuItem
        Action = View_Debug_Windows_Action
        object Watch1: TMenuItem
          Action = View_Watch_Action
        end
        object CallStack1: TMenuItem
          Action = View_CallStack_Action
        end
        object Memory1: TMenuItem
          Action = View_Memory_Action
        end
        object Variable1: TMenuItem
          Action = View_Variables_Action
        end
        object Register1: TMenuItem
          Action = View_Registers_Action
        end
        object Disassembly1: TMenuItem
          Action = View_Disassembly_Action
        end
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Refresh1: TMenuItem
        Action = View_Refresh_Action
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object Properties1: TMenuItem
        Action = View_Properties_Action
      end
    end
    object Insert1: TMenuItem
      Caption = '&Insert'
      object NewClass1: TMenuItem
        Action = Insert_New_Class_Action
      end
      object NewForm1: TMenuItem
        Action = Insert_New_Form_Action
      end
      object Resource1: TMenuItem
        Action = Insert_Resource_Action
      end
      object ResourceCopy1: TMenuItem
        Action = Insert_Resource_Copy_Action
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object FileAsText1: TMenuItem
        Action = Insert_File_As_Text_Action
      end
      object NewATLObject1: TMenuItem
        Action = Insert_New_ATL_Object_Action
      end
    end
    object Project1: TMenuItem
      Caption = '&Project'
      object SetActiveProject1: TMenuItem
        Action = Project_Set_Active_Project_Action
      end
      object AddToProject1: TMenuItem
        Action = Project_Add_To_Project_Action
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object SourceControl1: TMenuItem
        Action = Project_Source_Control_Action
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object DePendencies1: TMenuItem
        Action = Project_DePendencies_Action
      end
      object Settings1: TMenuItem
        Action = Project_Settings_Action
      end
      object DePendencies2: TMenuItem
        Action = Project_Export_Makefile_Action
      end
      object N20: TMenuItem
        Caption = '-'
      end
      object InsertProjectintoWorkspace1: TMenuItem
        Action = Project_Insert_Project_into_Workspace_Action
      end
    end
    object Build1: TMenuItem
      Caption = '&Build'
      object Compile1: TMenuItem
        Action = Build_Compile_Action
      end
      object Build2: TMenuItem
        Action = Build_Build_Action
      end
      object RebuildAll1: TMenuItem
        Action = Build_Rebuild_All_Action
      end
      object BatchBuild1: TMenuItem
        Action = Build_Batch_Build_Action
      end
      object Clean1: TMenuItem
        Action = Build_Clean_Action
      end
      object N21: TMenuItem
        Caption = '-'
      end
      object StartDebug1: TMenuItem
        Action = Build_Start_Debug_Action
      end
      object DebuggerRemoteConnection1: TMenuItem
        Action = Build_Debugger_Remote_Connection_Action
      end
      object N22: TMenuItem
        Caption = '-'
      end
      object Execute1: TMenuItem
        Action = Build_Execute_Action
      end
      object N23: TMenuItem
        Caption = '-'
      end
      object SetActiveConfiguration1: TMenuItem
        Action = Build_Set_Active_Configuration_Action
      end
      object Configurations1: TMenuItem
        Action = Build_Configurations_Action
      end
      object Profile1: TMenuItem
        Action = Build_Profile_Action
      end
    end
    object Debug1: TMenuItem
      Caption = '&Debug'
      object Go1: TMenuItem
        Action = Debug_Go_Action
      end
      object Restart1: TMenuItem
        Action = Debug_Restart_Action
      end
      object StopDebugging1: TMenuItem
        Action = Debug_Stop_Debugging_Action
      end
      object Break1: TMenuItem
        Action = Debug_Break_Action
      end
      object ApplyCodeChanges1: TMenuItem
        Action = Debug_Apply_Code_Changes_Action
      end
      object N33: TMenuItem
        Caption = '-'
      end
      object StepInto1: TMenuItem
        Action = Debug_Step_Into_Action
      end
      object StepOver1: TMenuItem
        Action = Debug_Step_Over_Action
      end
      object StepOut1: TMenuItem
        Action = Debug_Step_Out_Action
      end
      object RuntoCursor1: TMenuItem
        Action = Debug_Run_to_Cursor_Action
      end
      object StepIntoSpecificFunction1: TMenuItem
        Action = Debug_Step_Into_Specific_Function_Action
      end
      object N34: TMenuItem
        Caption = '-'
      end
      object Exceptions1: TMenuItem
        Action = Debug_Exceptions_Action
      end
      object hreads1: TMenuItem
        Action = Debug_Threads_Action
      end
      object Modules1: TMenuItem
        Action = Debug_Modules_Action
      end
      object N35: TMenuItem
        Caption = '-'
      end
      object ShowNextStatement1: TMenuItem
        Action = Debug_Show_Next_Statement_Action
      end
      object QuickWatch1: TMenuItem
        Action = Debug_QuickWatch_Action
      end
    end
    object Tools1: TMenuItem
      Caption = '&Tools'
      object SourceBrowser1: TMenuItem
        Action = Tools_Source_Browser_Action
      end
      object CloseSourceBrowserFile1: TMenuItem
        Action = Tools_Close_Source_Browser_File_Action
      end
      object N24: TMenuItem
        Caption = '-'
      end
      object VisualComponentManager1: TMenuItem
        Action = Tools_Visual_Component_Manager_Action
      end
      object ActiveXControlTestContainer1: TMenuItem
        Action = Tools_Register_Control_Action
      end
      object ErrorLookup1: TMenuItem
        Action = Tools_Error_Lookup_Action
      end
      object ActiveXControlTestContainer2: TMenuItem
        Action = Tools_ActiveX_Control_Test_Container_Action
      end
      object OLECOMObjectViewer1: TMenuItem
        Action = Tools_OLE_COM_Object_Viewer_Action
      end
      object SourceBrowser2: TMenuItem
        Action = Tools_Spy_Action
      end
      object MFCTracer1: TMenuItem
        Action = Tools_MFC_Tracer_Action
      end
      object N25: TMenuItem
        Caption = '-'
      end
      object Customize1: TMenuItem
        Action = Tools_Customize_Action
      end
      object Options1: TMenuItem
        Action = Tools_Options_Action
      end
      object N26: TMenuItem
        Caption = '-'
      end
      object Macro1: TMenuItem
        Action = Tools_Macro_Action
      end
      object RecordQuickMacro1: TMenuItem
        Action = Tools_Record_Quick_Macro_Action
      end
      object PlayQuickMacro1: TMenuItem
        Action = Tools_Play_Quick_Macro_Action
      end
    end
    object Window1: TMenuItem
      Caption = '&Window'
      object NewWindow1: TMenuItem
        Action = Window_New_Window_Action
      end
      object Spilt1: TMenuItem
        Action = Window_Spilt_Action
      end
      object DockingView1: TMenuItem
        Action = Window_Docking_View_Action
      end
      object Close1: TMenuItem
        Action = Window_Close_Action
      end
      object CloseAll1: TMenuItem
        Action = Window_Close_All_Action
      end
      object N27: TMenuItem
        Caption = '-'
      end
      object NewWindow2: TMenuItem
        Action = Window_Next_Action
      end
      object Previous1: TMenuItem
        Action = Window_Previous_Action
      end
      object N28: TMenuItem
        Caption = '-'
      end
      object Cascade1: TMenuItem
        Action = Window_Cascade_Action
      end
      object ileHorizontally1: TMenuItem
        Action = Window_Tile_Horizontally_Action
      end
      object ileVertically1: TMenuItem
        Action = Window_Tile_Vertically_Action
      end
      object N29: TMenuItem
        Caption = '-'
      end
      object Windows1: TMenuItem
        Action = Window_Windows_Action
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object Contents1: TMenuItem
        Action = Help_Contents_Action
      end
      object Search1: TMenuItem
        Action = Help_Search_Action
      end
      object Index1: TMenuItem
        Action = Help_Index_Action
      end
      object UseExtensionHelp1: TMenuItem
        Action = Help_Use_Extension_Help_Action
      end
      object N30: TMenuItem
        Caption = '-'
      end
      object KeyboardMap1: TMenuItem
        Action = Help_Keyboard_Map_Action
      end
      object N31: TMenuItem
        Caption = '-'
      end
      object echnicalSupport1: TMenuItem
        Action = Help_Tip_of_the_Day_Action
      end
      object echnicalSupport2: TMenuItem
        Action = Help_Technical_Support_Action
      end
      object MicrosofrontheWeb1: TMenuItem
        Action = Help_Microsofr_on_the_Web_Action
      end
      object N32: TMenuItem
        Caption = '-'
      end
      object AboutVisualC1: TMenuItem
        Action = Help_About_Visual_Cpp_Action
      end
    end
  end
  object MainActionList: TActionList
    Images = Action_ImageList
    Left = 72
    Top = 104
    object File_New_Action: TAction
      Category = 'File_Category'
      Caption = '&New...'
      ImageIndex = 0
      ShortCut = 16462
      OnExecute = File_New_ActionExecute
    end
    object File_Open_Action: TAction
      Category = 'File_Category'
      Caption = '&Open...'
      ImageIndex = 1
      ShortCut = 16463
      OnExecute = File_Open_ActionExecute
    end
    object File_Close_Action: TAction
      Category = 'File_Category'
      Caption = '&Close'
      OnExecute = File_New_ActionExecute
    end
    object File_Open_WorkSpace_Action: TAction
      Category = 'File_Category'
      Caption = 'Open &WorkSpace...'
      OnExecute = File_New_ActionExecute
    end
    object File_Save_WorKSpace_Action: TAction
      Category = 'File_Category'
      Caption = 'Sa&ve WorkSpace'
      OnExecute = File_New_ActionExecute
    end
    object File_Close_WorKSpace_Action: TAction
      Category = 'File_Category'
      Caption = 'Close Wor&kSpace'
      OnExecute = File_New_ActionExecute
    end
    object File_Save_Action: TAction
      Category = 'File_Category'
      Caption = '&Save'
      ImageIndex = 2
      ShortCut = 16467
      OnExecute = File_New_ActionExecute
    end
    object File_SaveAs_Action: TAction
      Category = 'File_Category'
      Caption = 'Save&As...'
      OnExecute = File_New_ActionExecute
    end
    object File_SaveAll_Action: TAction
      Category = 'File_Category'
      Caption = 'SaveA&ll'
      ImageIndex = 3
      OnExecute = File_New_ActionExecute
    end
    object File_Page_Setup_Action: TAction
      Category = 'File_Category'
      Caption = 'Page Set&up...'
      OnExecute = File_New_ActionExecute
    end
    object File_Print_Action: TAction
      Category = 'File_Category'
      Caption = '&Print...'
      ShortCut = 16464
      OnExecute = File_New_ActionExecute
    end
    object File_Recent_Files_Action: TAction
      Category = 'File_Category'
      Caption = 'Recent &Files'
      OnExecute = File_New_ActionExecute
    end
    object File_Recent_WorkSpace_Action: TAction
      Category = 'File_Category'
      Caption = 'Recent Wo&rkSpace'
      OnExecute = File_New_ActionExecute
    end
    object File_Exit_Action: TAction
      Category = 'File_Category'
      Caption = 'E&xit'
      OnExecute = File_Exit_ActionExecute
    end
    object Edit_Find_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Find...'
      ImageIndex = 19
      ShortCut = 16454
      OnExecute = File_New_ActionExecute
    end
    object Edit_Find_In_File_Action: TAction
      Category = 'Edit_Category'
      Caption = 'F&ind In File...'
      ImageIndex = 13
      OnExecute = File_New_ActionExecute
    end
    object Edit_Replace_Action: TAction
      Category = 'Edit_Category'
      Caption = 'R&eplace...'
      ShortCut = 16456
      OnExecute = File_New_ActionExecute
    end
    object Edit_Goto_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Go To..'
      OnExecute = File_New_ActionExecute
    end
    object Edit_Bookmarks_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Bookmarks...'
      ShortCut = 16455
      OnExecute = File_New_ActionExecute
    end
    object View_Resource_Symbols_Action: TAction
      Category = 'View_Category'
      Caption = 'Resource S&ymbols...'
      OnExecute = File_New_ActionExecute
    end
    object View_Resource_Include_Action: TAction
      Category = 'View_Category'
      Caption = 'Resource I&nclude...'
      OnExecute = File_New_ActionExecute
    end
    object View_Full_Screen_Action: TAction
      Category = 'View_Category'
      Caption = 'F&ull Screen'
      OnExecute = File_New_ActionExecute
    end
    object View_Workspace_Action: TAction
      Category = 'View_Category'
      Caption = 'Wor&kspace'
      ImageIndex = 10
      ShortCut = 32816
      OnExecute = View_Workspace_ActionExecute
    end
    object View_OutPut_Action: TAction
      Category = 'View_Category'
      Caption = '&OutPut'
      ImageIndex = 11
      ShortCut = 32818
      OnExecute = View_OutPut_ActionExecute
    end
    object View_Debug_Windows_Action: TAction
      Category = 'View_Category'
      Caption = '&Debug Windows'
      OnExecute = View_Debug_Windows_ActionExecute
    end
    object View_Refresh_Action: TAction
      Category = 'View_Category'
      Caption = 'Refres&h'
      OnExecute = File_New_ActionExecute
    end
    object View_Properties_Action: TAction
      Category = 'View_Category'
      Caption = '&Properties'
      OnExecute = File_New_ActionExecute
    end
    object Edit_Advanced_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Advanced'
      OnExecute = File_New_ActionExecute
    end
    object Edit_Breakpoints_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Brea&kpoints...'
      OnExecute = File_New_ActionExecute
    end
    object Edit_List_Members_Action: TAction
      Category = 'Edit_Category'
      Caption = 'List &Members'
      OnExecute = File_New_ActionExecute
    end
    object Edit_Type_Info_Action: TAction
      Category = 'Edit_Category'
      Caption = 'T&ype Info'
      OnExecute = File_New_ActionExecute
    end
    object Edit_Parameter_Info_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Parameter &Info'
      OnExecute = File_New_ActionExecute
    end
    object Edit_Complete_Word_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Complete W&ord'
      OnExecute = File_New_ActionExecute
    end
    object Insert_New_Class_Action: TAction
      Category = 'Insert_Category'
      Caption = '&New Class...'
      OnExecute = File_New_ActionExecute
    end
    object Insert_New_Form_Action: TAction
      Category = 'Insert_Category'
      Caption = 'New F&orm...'
      OnExecute = File_New_ActionExecute
    end
    object Insert_Resource_Action: TAction
      Category = 'Insert_Category'
      Caption = '&Resource'
      ShortCut = 16466
      OnExecute = File_New_ActionExecute
    end
    object Insert_Resource_Copy_Action: TAction
      Category = 'Insert_Category'
      Caption = 'Resource Copy...'
      OnExecute = File_New_ActionExecute
    end
    object Insert_File_As_Text_Action: TAction
      Category = 'Insert_Category'
      Caption = 'File As Text...'
      OnExecute = File_New_ActionExecute
    end
    object Insert_New_ATL_Object_Action: TAction
      Category = 'Insert_Category'
      Caption = 'New ATL Object...'
      ImageIndex = 48
      OnExecute = File_New_ActionExecute
    end
    object Project_Set_Active_Project_Action: TAction
      Category = 'Project_Category'
      Caption = 'Set Acti&ve Project'
      OnExecute = File_New_ActionExecute
    end
    object Project_Add_To_Project_Action: TAction
      Category = 'Project_Category'
      Caption = '&Add To Project'
      OnExecute = File_New_ActionExecute
    end
    object Project_Source_Control_Action: TAction
      Category = 'Project_Category'
      Caption = 'Source &Control'
      OnExecute = File_New_ActionExecute
    end
    object Project_DePendencies_Action: TAction
      Category = 'Project_Category'
      Caption = 'D&ePendencies...'
      OnExecute = File_New_ActionExecute
    end
    object Project_Settings_Action: TAction
      Category = 'Project_Category'
      Caption = '&Settings...'
      ShortCut = 32886
      OnExecute = File_New_ActionExecute
    end
    object Project_Export_Makefile_Action: TAction
      Category = 'Project_Category'
      Caption = 'Export &Makefile'
      OnExecute = File_New_ActionExecute
    end
    object Project_Insert_Project_into_Workspace_Action: TAction
      Category = 'Project_Category'
      Caption = '&Insert Project into Workspace'
      OnExecute = File_New_ActionExecute
    end
    object Build_Compile_Action: TAction
      Category = 'Build_Category'
      Caption = '&Compile...'
      ImageIndex = 23
      ShortCut = 16502
      OnExecute = File_New_ActionExecute
    end
    object Build_Build_Action: TAction
      Category = 'Build_Category'
      Caption = '&Build'
      ImageIndex = 24
      ShortCut = 16504
      OnExecute = File_New_ActionExecute
    end
    object Build_Rebuild_All_Action: TAction
      Category = 'Build_Category'
      Caption = '&Rebuild All'
      OnExecute = File_New_ActionExecute
    end
    object Build_Batch_Build_Action: TAction
      Category = 'Build_Category'
      Caption = 'Batch B&uild...'
      OnExecute = File_New_ActionExecute
    end
    object Build_Clean_Action: TAction
      Category = 'Build_Category'
      Caption = 'Cl&ean'
      OnExecute = File_New_ActionExecute
    end
    object Build_Start_Debug_Action: TAction
      Category = 'Build_Category'
      Caption = 'Start &Debug'
      OnExecute = File_New_ActionExecute
    end
    object Build_Debugger_Remote_Connection_Action: TAction
      Category = 'Build_Category'
      Caption = 'Debugger Remote Co&nnection...'
      OnExecute = File_New_ActionExecute
    end
    object Build_Execute_Action: TAction
      Category = 'Build_Category'
      Caption = 'E&xecute'
      ImageIndex = 26
      ShortCut = 16500
      OnExecute = File_New_ActionExecute
    end
    object Build_Set_Active_Configuration_Action: TAction
      Category = 'Build_Category'
      Caption = 'Set Active C&onfiguration...'
      OnExecute = File_New_ActionExecute
    end
    object Build_Configurations_Action: TAction
      Category = 'Build_Category'
      Caption = 'Con&figurations...'
      OnExecute = File_New_ActionExecute
    end
    object Build_Profile_Action: TAction
      Category = 'Build_Category'
      Caption = '&Profile...'
      OnExecute = File_New_ActionExecute
    end
    object Tools_Source_Browser_Action: TAction
      Category = 'Tools_Category'
      Caption = 'Source Bro&wser...'
      ShortCut = 32891
      OnExecute = File_New_ActionExecute
    end
    object Tools_Close_Source_Browser_File_Action: TAction
      Category = 'Tools_Category'
      Caption = 'Close Source Browser &File'
      OnExecute = File_New_ActionExecute
    end
    object Tools_Visual_Component_Manager_Action: TAction
      Category = 'Tools_Category'
      Caption = 'Visual Component M&anager'
      ImageIndex = 49
      OnExecute = File_New_ActionExecute
    end
    object Tools_Register_Control_Action: TAction
      Category = 'Tools_Category'
      Caption = 'Re&gister Control'
      ImageIndex = 50
      OnExecute = File_New_ActionExecute
    end
    object Tools_Error_Lookup_Action: TAction
      Category = 'Tools_Category'
      Caption = 'Error Loo&kup'
      ImageIndex = 51
      OnExecute = File_New_ActionExecute
    end
    object Tools_ActiveX_Control_Test_Container_Action: TAction
      Category = 'Tools_Category'
      Caption = 'Activ&eX Control Test Container'
      ImageIndex = 52
      OnExecute = File_New_ActionExecute
    end
    object Tools_OLE_COM_Object_Viewer_Action: TAction
      Category = 'Tools_Category'
      Caption = 'OLE/COM Object &Viewer'
      ImageIndex = 53
      OnExecute = File_New_ActionExecute
    end
    object Tools_Spy_Action: TAction
      Category = 'Tools_Category'
      Caption = 'S&py++'
      ImageIndex = 54
      OnExecute = File_New_ActionExecute
    end
    object Tools_MFC_Tracer_Action: TAction
      Category = 'Tools_Category'
      Caption = 'MFC &Tracer'
      ImageIndex = 55
      OnExecute = File_New_ActionExecute
    end
    object Tools_Customize_Action: TAction
      Category = 'Tools_Category'
      Caption = '&Customize...'
      OnExecute = File_New_ActionExecute
    end
    object Tools_Options_Action: TAction
      Category = 'Tools_Category'
      Caption = '&Options...'
      OnExecute = File_New_ActionExecute
    end
    object Tools_Macro_Action: TAction
      Category = 'Tools_Category'
      Caption = '&Macro...'
      ImageIndex = 56
      OnExecute = File_New_ActionExecute
    end
    object Tools_Record_Quick_Macro_Action: TAction
      Category = 'Tools_Category'
      Caption = '&Record Quick Macro'
      ShortCut = 24658
      OnExecute = File_New_ActionExecute
    end
    object Tools_Play_Quick_Macro_Action: TAction
      Category = 'Tools_Category'
      Caption = 'P&lay Quick Macro'
      ShortCut = 24656
      OnExecute = File_New_ActionExecute
    end
    object Window_New_Window_Action: TAction
      Category = 'Window_Category'
      Caption = '&New Window'
      ImageIndex = 57
      OnExecute = File_New_ActionExecute
    end
    object Window_Spilt_Action: TAction
      Category = 'Window_Category'
      Caption = 'S&pilt'
      ImageIndex = 58
      OnExecute = File_New_ActionExecute
    end
    object Window_Docking_View_Action: TAction
      Category = 'Window_Category'
      Caption = '&Docking_View'
      ShortCut = 32885
      OnExecute = File_New_ActionExecute
    end
    object Window_Close_Action: TAction
      Category = 'Window_Category'
      Caption = 'Cl&ose'
      ImageIndex = 59
      OnExecute = Window_Close_ActionExecute
    end
    object Window_Close_All_Action: TAction
      Category = 'Window_Category'
      Caption = 'Close Al&l'
      OnExecute = Window_Close_All_ActionExecute
    end
    object Window_Next_Action: TAction
      Category = 'Window_Category'
      Caption = 'Ne&xt'
      ImageIndex = 60
      OnExecute = Window_Next_ActionExecute
    end
    object Window_Previous_Action: TAction
      Category = 'Window_Category'
      Caption = 'Pre&vious'
      ImageIndex = 61
      OnExecute = Window_Previous_ActionExecute
    end
    object Window_Cascade_Action: TAction
      Category = 'Window_Category'
      Caption = '&Cascade'
      ImageIndex = 62
      OnExecute = Window_Cascade_ActionExecute
    end
    object Window_Tile_Horizontally_Action: TAction
      Category = 'Window_Category'
      Caption = 'Tile &Horizontally'
      ImageIndex = 63
      OnExecute = Window_Tile_Horizontally_ActionExecute
    end
    object Window_Tile_Vertically_Action: TAction
      Category = 'Window_Category'
      Caption = '&Tile Vertically'
      ImageIndex = 64
      OnExecute = Window_Tile_Vertically_ActionExecute
    end
    object Window_Windows_Action: TAction
      Category = 'Window_Category'
      Caption = '&Windows'
      ImageIndex = 65
      OnExecute = File_New_ActionExecute
    end
    object Help_Contents_Action: TAction
      Category = 'Help_Category'
      Caption = '&Contents...'
      OnExecute = File_New_ActionExecute
    end
    object Help_Search_Action: TAction
      Category = 'Help_Category'
      Caption = '&Search...'
      ImageIndex = 14
      OnExecute = File_New_ActionExecute
    end
    object Help_Index_Action: TAction
      Category = 'Help_Category'
      Caption = '&Index...'
      OnExecute = File_New_ActionExecute
    end
    object Help_Use_Extension_Help_Action: TAction
      Category = 'Help_Category'
      Caption = '&Use Extension Help'
      OnExecute = File_New_ActionExecute
    end
    object Help_Tip_of_the_Day_Action: TAction
      Category = 'Help_Category'
      Caption = 'Ti&p of the Day...'
      OnExecute = File_New_ActionExecute
    end
    object Help_Keyboard_Map_Action: TAction
      Category = 'Help_Category'
      Caption = '&Keyboard Map...'
      OnExecute = File_New_ActionExecute
    end
    object Help_Technical_Support_Action: TAction
      Category = 'Help_Category'
      Caption = '&Technical Support'
      OnExecute = File_New_ActionExecute
    end
    object Help_Microsofr_on_the_Web_Action: TAction
      Category = 'Help_Category'
      Caption = 'Microsofr on the &Web'
      OnExecute = File_New_ActionExecute
    end
    object Help_About_Visual_Cpp_Action: TAction
      Category = 'Help_Category'
      Caption = '&About Visual C++'
      ImageIndex = 47
      OnExecute = File_New_ActionExecute
    end
    object Debug_Go_Action: TAction
      Category = 'Debug_Category'
      Caption = '&Go'
      ImageIndex = 27
      ShortCut = 120
      OnExecute = File_New_ActionExecute
    end
    object Debug_Restart_Action: TAction
      Category = 'Debug_Category'
      Caption = '&Restart'
      ImageIndex = 74
      ShortCut = 16497
      OnExecute = File_New_ActionExecute
    end
    object Debug_Stop_Debugging_Action: TAction
      Category = 'Debug_Category'
      Caption = 'Stop &Debugging'
      ImageIndex = 30
      ShortCut = 8308
      OnExecute = File_New_ActionExecute
    end
    object Debug_Break_Action: TAction
      Category = 'Debug_Category'
      Caption = '&Break'
      ImageIndex = 31
      OnExecute = File_New_ActionExecute
    end
    object Debug_Apply_Code_Changes_Action: TAction
      Category = 'Debug_Category'
      Caption = 'Appl&y Code Changes'
      ImageIndex = 32
      ShortCut = 32889
      OnExecute = File_New_ActionExecute
    end
    object Debug_Step_Into_Action: TAction
      Category = 'Debug_Category'
      Caption = 'Step &Into'
      ImageIndex = 34
      ShortCut = 118
      OnExecute = File_New_ActionExecute
    end
    object Debug_Step_Over_Action: TAction
      Category = 'Debug_Category'
      Caption = 'Step &Over'
      ImageIndex = 35
      ShortCut = 119
      OnExecute = File_New_ActionExecute
    end
    object Debug_Step_Out_Action: TAction
      Category = 'Debug_Category'
      Caption = 'Step O&ut'
      ImageIndex = 36
      ShortCut = 8314
      OnExecute = File_New_ActionExecute
    end
    object Debug_Run_to_Cursor_Action: TAction
      Category = 'Debug_Category'
      Caption = 'Run to &Cursor'
      ImageIndex = 37
      ShortCut = 16505
      OnExecute = File_New_ActionExecute
    end
    object Debug_Step_Into_Specific_Function_Action: TAction
      Category = 'Debug_Category'
      Caption = 'Ste&p Into Specific Function'
      OnExecute = File_New_ActionExecute
    end
    object Debug_Exceptions_Action: TAction
      Category = 'Debug_Category'
      Caption = '&Exceptions...'
      OnExecute = File_New_ActionExecute
    end
    object Debug_Threads_Action: TAction
      Category = 'Debug_Category'
      Caption = '&Threads...'
      OnExecute = File_New_ActionExecute
    end
    object Debug_Modules_Action: TAction
      Category = 'Debug_Category'
      Caption = '&Modules...'
      OnExecute = File_New_ActionExecute
    end
    object Debug_Show_Next_Statement_Action: TAction
      Category = 'Debug_Category'
      Caption = 'Show &Next Statement'
      ImageIndex = 33
      OnExecute = File_New_ActionExecute
    end
    object Debug_QuickWatch_Action: TAction
      Category = 'Debug_Category'
      Caption = '&QuickWatch...'
      ImageIndex = 38
      ShortCut = 8312
      OnExecute = File_New_ActionExecute
    end
    object Edit_Toggle_Bookmark_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Toggle Bookmark'
      ImageIndex = 15
      ShortCut = 16497
      OnExecute = File_New_ActionExecute
    end
    object Edit_Next_Bookmark_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Next Bookmark'
      ImageIndex = 16
      OnExecute = File_New_ActionExecute
    end
    object Edit_Previous_Bookmark_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Previous Bookmark'
      ImageIndex = 17
      OnExecute = File_New_ActionExecute
    end
    object Edit_Clear_All_BookMarks_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Clear All BookMarks'
      ImageIndex = 18
      OnExecute = File_New_ActionExecute
    end
    object Edit_Increase_Indent_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Increase Indent'
      ImageIndex = 20
      OnExecute = File_New_ActionExecute
    end
    object Edit_Decrease_Indent_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Decrease Indent'
      ImageIndex = 21
      OnExecute = File_New_ActionExecute
    end
    object Edit_Toggle_WhiteSpace_Display_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Toggle WhiteSpace Display'
      ImageIndex = 22
      OnExecute = File_New_ActionExecute
    end
    object View_Watch_Action: TAction
      Category = 'View_Category'
      Caption = '&Watch'
      ImageIndex = 66
      ShortCut = 32819
      OnExecute = View_Watch_ActionExecute
    end
    object View_Variables_Action: TAction
      Category = 'View_Category'
      Caption = '&Variable'
      ImageIndex = 67
      ShortCut = 32820
      OnExecute = View_Variables_ActionExecute
    end
    object View_Registers_Action: TAction
      Category = 'View_Category'
      Caption = '&Registers'
      ImageIndex = 68
      ShortCut = 32821
      OnExecute = View_Registers_ActionExecute
    end
    object View_Memory_Action: TAction
      Category = 'View_Category'
      Caption = '&Memory'
      ImageIndex = 69
      ShortCut = 32822
      OnExecute = View_Memory_ActionExecute
    end
    object View_CallStack_Action: TAction
      Category = 'View_Category'
      Caption = '&Call Stack'
      ImageIndex = 70
      ShortCut = 32823
      OnExecute = View_CallStack_ActionExecute
    end
    object View_Disassembly_Action: TAction
      Category = 'View_Category'
      Caption = '&Disassembly'
      ImageIndex = 71
      ShortCut = 32824
      OnExecute = File_New_ActionExecute
    end
    object Build_Stop_Build_Action: TAction
      Category = 'Build_Category'
      Caption = 'Stop Build'
      ImageIndex = 72
      OnExecute = File_New_ActionExecute
    end
    object Build_Insert_Remove_Breakpoint_Action: TAction
      Category = 'Build_Category'
      Caption = 'Insert/Remove Breakpoint'
      ImageIndex = 73
      OnExecute = File_New_ActionExecute
    end
    object Edit_Cut_Action: TEditCut
      Category = 'Edit_Category'
      Caption = 'Cu&t'
      Hint = 'Cut|Cuts the selection and puts it on the Clipboard'
      ImageIndex = 75
      ShortCut = 16472
      OnExecute = Edit_Cut_ActionExecute
      OnUpdate = Edit_Cut_ActionUpdate
    end
    object Edit_Copy_Action: TEditCopy
      Category = 'Edit_Category'
      Caption = '&Copy'
      Hint = 'Copy|Copies the selection and puts it on the Clipboard'
      ImageIndex = 76
      ShortCut = 16451
      OnExecute = Edit_Copy_ActionExecute
      OnUpdate = Edit_Copy_ActionUpdate
    end
    object Edit_Paste_Action: TEditPaste
      Category = 'Edit_Category'
      Caption = '&Paste'
      Hint = 'Paste|Inserts Clipboard contents'
      ImageIndex = 77
      ShortCut = 16470
      OnExecute = Edit_Paste_ActionExecute
      OnUpdate = Edit_Paste_ActionUpdate
    end
    object Edit_SelectAll_Action: TEditSelectAll
      Category = 'Edit_Category'
      Caption = 'Select &All'
      Hint = 'Select All|Selects the entire document'
      ShortCut = 16449
      OnExecute = Edit_SelectAll_ActionExecute
    end
    object Edit_Undo_Action: TEditUndo
      Category = 'Edit_Category'
      Caption = '&Undo'
      Hint = 'Undo|Reverts the last action'
      ImageIndex = 8
      ShortCut = 16474
      OnExecute = Edit_Undo_ActionExecute
      OnUpdate = Edit_Undo_ActionUpdate
    end
    object Edit_Redo_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Redo'
      ImageIndex = 9
      ShortCut = 16473
      OnExecute = Edit_Redo_ActionExecute
      OnUpdate = Edit_Redo_ActionUpdate
    end
    object Edit_Delete_Action: TEditDelete
      Category = 'Edit_Category'
      Caption = '&Delete'
      Hint = 'Delete|Erases the selection'
      ImageIndex = 7
      ShortCut = 46
      OnExecute = Edit_Delete_ActionExecute
      OnUpdate = Edit_Delete_ActionUpdate
    end
    object View_Standard_Toolbar_Action: TAction
      Category = 'View_Category'
      Caption = 'Standard'
      OnExecute = View_Standard_Toolbar_ActionExecute
    end
    object View_Debug_Toolbar_Action: TAction
      Category = 'View_Category'
      Caption = 'Debug'
      OnExecute = View_Debug_Toolbar_ActionExecute
    end
    object View_Build_Minibar_Toolbar_Action: TAction
      Category = 'View_Category'
      Caption = 'Build Minibar'
      OnExecute = View_Build_Minibar_Toolbar_ActionExecute
    end
    object View_Edit_Toolbar_Action: TAction
      Category = 'View_Category'
      Caption = 'Edit'
      OnExecute = View_Edit_Toolbar_ActionExecute
    end
    object View_Customize_Toolbar_Action: TAction
      Category = 'View_Category'
      Caption = 'Customize...'
      OnExecute = File_New_ActionExecute
    end
  end
  object Action_ImageList: TImageList
    Left = 8
    Top = 104
    Bitmap = {
      494C010150005400040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000005001000001002000000000000050
      0100000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF000000000000000000000000000000
      00000000000000000000FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00840000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00000000000000000000000000FFFF
      FF00840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000000000000000
      00000000000000000000FFFFFF00840000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000FFFF
      FF00840000008400000084000000840000000000000000848400848484000084
      8400848484000084840084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000084848400008484008484
      8400008484008484840000848400848484000084840084848400008484008484
      8400008484000000000000000000000000000000000000000000840000008400
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000848400848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000840000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000FFFFFF0000000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000084848400848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400848484000084
      84000000000000FFFF00000000000000000000FFFF0000000000848484000084
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400840000008400
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484840084000000848484000000
      0000840000008484840000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000008400
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      00008400000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0084848400FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000084000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      00008400000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF000000840084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000084000000840084848400000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000084000000FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840084848400848484000000000000000000000084000000
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000008400848484008484840000008400000084000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000084000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000008400000084000000840000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00848484000000
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000008400000084000000840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000084000000FFFFFF00848484008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000840000008400000084000000840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000084000000840084848400000000000000840000008400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      00000000000000000000000000000000000084848400FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000084008484
      8400848484000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      8400848484000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484840084000000848484000000
      0000840000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400840000008400
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000840000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000008400000000000000000000000000
      0000FFFFFF00FFFFFF0000000000848484008484840000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000008484
      84000000000084848400FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0084848400FFFF
      FF008484840084848400848484008484840084848400FFFFFF00848484008484
      8400FFFFFF000000000000000000000000008400000000000000000000000000
      0000FFFFFF008484840000000000FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000FFFF0000000000008484840000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00000000000000
      000000000000FFFFFF000000000000000000FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000008400000000000000000000000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000000000008484840000000000FFFF
      00000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0084848400FFFF
      FF008484840084848400848484008484840084848400FFFFFF00848484008484
      8400FFFFFF000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000FFFFFF0084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000008400000000000000000000000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0084848400FFFF
      FF008484840084848400848484008484840084848400FFFFFF00848484008484
      8400FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF008484840084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00848484008484
      840084848400FFFFFF008484840084848400FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0084848400FFFF
      FF008484840084848400848484008484840084848400FFFFFF00848484008484
      8400FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000008484840084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF0084848400848484008484840084848400FFFFFF008484840084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      000084000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF00840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      00008400000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      00008400000000000000000000000000000000000000FFFF0000FFFFFF000000
      0000000000000000000000000000FFFF0000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      000084000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF008400000000000000000000000000000000000000FFFF0000FFFF00000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000FF000000FF00
      00008400000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      000084000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000848484000000
      0000000000000000000000000000FFFFFF00848484000000000084848400FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000FF000000FFFF
      FF00840000000000000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000008484
      84000000000000000000FFFFFF00FFFFFF000000000000000000000000008484
      8400FFFFFF00FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000FF000000FFFF
      FF00840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000FFFFFF000000000000000000FFFF0000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FFFF00000000000000
      0000FFFFFF00FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF0000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF000000000000000000FFFFFF00000000000000000000000000FFFF00000000
      0000848484008484840084848400FFFFFF00848484000000000000FFFF000000
      0000FFFFFF00FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000008400000084000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF0000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000008400000000000000FF0000000000
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      000084000000840000008400000084000000000000000000000000000000FFFF
      FF00848484008484840084848400FFFFFF008484840084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF008484840084848400FFFFFF008484840084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084000000000000000000000084000000FFFFFF00FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      000084000000840000008400000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008400000000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000008400000000000000000000000000
      000084000000840000008400000084848400000000000000000000000000FFFF
      FF0084848400848484008484840084848400FFFFFF008484840084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00848484008484840084848400FFFFFF008484840084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000084000000FFFFFF008400
      000084000000840000008400000084000000FFFFFF0084000000840000008400
      00008400000084000000000000000000000084000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF008400000000000000000000000000
      000084000000000000000000000084000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008400000000000000840000008400
      0000840000008400000084000000840000008400000000000000000000000000
      0000000000000000000084848400840000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000000000000000000000000
      0000000000000000000084000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF00000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00FFFFFF0084000000FFFFFF00840000008400000084000000840000008400
      0000840000008400000084000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00FFFFFF008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000084000000FFFFFF008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF00000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000
      0000FFFFFF00FFFFFF00FFFFFF00000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084000000000000000000000000000000FFFFFF00848484008484
      840000000000000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000000000
      00008484840084848400FFFFFF000000000084000000FFFFFF0084000000FFFF
      FF00840000008400000084000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084000000FFFFFF00840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084000000000000000000000000000000FFFFFF00848484008484
      8400848484008484840084848400FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0084848400848484008484
      84008484840084848400FFFFFF000000000084000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      00008400000084000000840000008400000084000000FFFFFF00840000008400
      0000840000008400000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840000000000000000000000000000000000000000008484
      8400848484008484840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000840000008400000084
      0000008400008484840000000000000000000000000000000000000000000084
      0000008400000084000000840000848484000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000840000008400000084
      0000008400008484840000000000000000000000000000000000000000000084
      0000008400000084000000840000848484000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000840000000000000084
      0000008400008484840000000000000000000000000000000000000000000084
      0000000000000084000000840000848484000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000FFFFFF008484
      8400848484008484840084848400848484008484840084848400FFFFFF000000
      0000000000000000000000000000000000000000000000840000008400000084
      0000008400008484840000000000000000000000000000000000000000000084
      0000008400000084000000840000848484000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000008484000084840000848400008484
      0000848400008484000084000000840000008400000084000000840000008400
      000084000000840000008400000084000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000FFFFFF008484
      84008484840084848400848484008484840084848400FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000084848400000000000000
      00000000000000000000000000000000000084840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF008400000084000000840000008400
      000084000000840000008400000084000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000000000008484840000000000000000000000
      00000000000000000000000000000000000084840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000840000008484
      8400FFFFFF0084848400FFFFFF0084848400FFFFFF0084848400FFFFFF008484
      8400FFFFFF008484840084000000000000000000000000000000FFFFFF008484
      8400848484008484840084848400848484008484840084848400FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400000000000000000000000000848484000000000000000000000000000000
      00000000000000000000000000000000000084840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084840000000000000000
      000000000000000000000000000000000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000000000008484840000FF
      0000848484000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000008484000084840000848400008484
      0000848400008484000084840000848400008484000084840000848400008484
      000084840000848400008484000000000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      000000000000000000000000000000000000000000008484840000FF000000FF
      000000FF00008484840000000000000000000000000000000000000000008484
      84008484840084848400848484008484840084840000FFFFFF00848400008484
      0000848400008484000084840000848400008484000084840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008484000000000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000000000000000000000008400000000008484840000FF000000FF000000FF
      000000FF000000FF000084848400000000000000000000000000000000000084
      0000008400000084000000840000848484008484000084840000848400008484
      0000848400008484000084840000848400008484000084840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008484000000000000000000000000000084000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000008484840000FF000000FF000000FF
      000000FF000000FF000084848400848484008484840084848400848484000084
      0000008400000084000000840000848484000000000000000000000000000000
      00000000000084840000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084840000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      840000000000000084000000000000000000000000008484840000FF000000FF
      000000FF00008484840000000000000000000000000000000000000000000084
      0000000000000084000000840000848484000000000000000000000000000000
      0000000000008484000084840000848400008484000084840000848400008484
      000084840000848400008484000000000000000000000000000084000000FFFF
      FF00840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      84000000840000000000000000000000000000000000000000008484840000FF
      0000848484000000000000000000000000000000000000000000000000000084
      0000008400000084000000840000848484000000000000000000000000000000
      00000000000084840000FFFFFF00848400008484000084840000848400008484
      0000848400008484000084840000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000084000000840000000000000000000000000000000000000000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484000084840000848400008484000084840000848400008484
      0000848400008484000084840000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008484000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008484000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008484000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008484000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484000000000000000000000000000000
      0000000000000000000084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000008484840000000000848484000000000000000000000000000000
      0000000000000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000000000000000000000000000
      0000008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000840000000000000000000000
      0000008484000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000008484840000000000848484000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000008484840000000000848484000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008484000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008484000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000008484000084840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FF00FFFFFF00FFFFFF000000FF000000
      FF00FFFFFF00FFFFFF000000FF00000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF008484840084000000840000008400000084848400FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF000000FF000000000084848400FFFFFF00FFFF
      FF008484840084000000FFFFFF00FFFFFF00FFFFFF008400000084848400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000FF000000FF000000FF000000FF00FFFFFF00FFFF
      FF000000FF000000FF000000FF000000FF008400000084848400FFFFFF008484
      840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFF
      FF00848484000000840000008400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      84000000000084848400FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000008484840000000000000000000000
      000000000000000000000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000FF000000FF000000FF000000000084000000840000008400
      0000FFFFFF00FFFFFF000000840084848400FFFFFF00FFFFFF00FFFFFF008484
      8400000084000000000000000000000084000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000FFFF0000000000008484840000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000FF000000FF00FFFFFF00FFFFFF000000FF000000
      FF00FFFFFF00FFFFFF000000FF000000FF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000840084848400FFFFFF00848484000000
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000000000FFFF
      00000000000000000000FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00000000000000
      00000000000000000000000000000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF000000FF00000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000084000000840000008400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000FFFFFF0084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000FF000000FF000000FF000000
      FF000000FF000000FF0000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00000000000000
      0000000000000000000000000000FFFFFF00000000000000FF000000FF000000
      FF000000FF000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF008484840084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FFFF000000000000000000000000000000000000FFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FFFF00000000000000000000000000000000000000000000FFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0084848400848484008484840084848400FFFFFF008484840084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000000000000000000000000000FFFF0000FFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00000000000000
      0000000000000000000000000000FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000840000008400000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000008484840084848400848484008484
      8400848484008484840084848400848484008484840000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000840000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000008484840000000000848484000000
      0000000000000000000000000000FFFFFF00848484000000000084848400FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000008400000000000000000000000000
      0000FFFFFF00FFFFFF0000000000848484008484840000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000008484
      84000000000000000000FFFFFF00FFFFFF000000000000000000000000008484
      8400FFFFFF00FFFFFF00000000000000000000000000FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0084848400FFFF
      FF008484840084848400848484008484840084848400FFFFFF00848484008484
      8400FFFFFF000000000000000000000000008400000000000000000000000000
      0000FFFFFF008484840000000000FFFFFF00FFFFFF0000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FFFF00000000000000
      0000FFFFFF00FFFFFF00000000000000000000000000FFFFFF00000000000000
      000000000000FFFFFF000000000000000000FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000008400000000000000000000000000
      0000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFF00000000
      0000848484008484840084848400FFFFFF00848484000000000000FFFF000000
      0000FFFFFF00FFFFFF00000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0084848400FFFF
      FF008484840084848400848484008484840084848400FFFFFF00848484008484
      8400FFFFFF000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000008400000000000000000000000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF008484840084848400FFFFFF008484840084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0084848400FFFF
      FF008484840084848400848484008484840084848400FFFFFF00848484008484
      8400FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00848484008484840084848400FFFFFF008484840084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00848484008484
      840084848400FFFFFF008484840084848400FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF0084848400FFFF
      FF008484840084848400848484008484840084848400FFFFFF00848484008484
      8400FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF000000
      000000000000000000000000000000000000000000008484840084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      00008400000084000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFFFF000000
      0000000000000000000000000000FFFF0000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF00000000
      00000000000000000000000000000000000000000000FFFF0000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF00000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF000000000000FFFF
      000000000000000000000000000000000000FFFF000000000000FFFF00000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFF00000000
      000000000000000000000000000000000000FFFFFF00FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000000000000000000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000840000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00848484008484840084848400FFFFFF008484840084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF0084848400848484008484840084848400FFFFFF008484840084848400FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000008484840084000000000000000000000000000000000000000000
      0000840000008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000084848400840000008400000084000000840000008400
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00848484008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF0084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008400000084000000840000000000
      0000840000008400000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000000000000000000000000000084848400FFFFFF00FFFFFF000000
      0000000000000000000000000000000000008400000084000000848484000000
      0000840000008400000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      00008400000084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      00008400000084000000840000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00848484000000
      0000000000008400000084000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000084000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000000000000000000000000000084848400FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008484840000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000848484000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000848484000000000000000000000000000000
      0000000000000000000000000000000000008484840084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000000000000C6C6C600C6C6C600C6C6C6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008400000000000000C6C6
      C6008400000000000000C6C6C600C6C6C6008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000C6C6
      C600C6C6C60084000000C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      00000000000084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484008400
      0000FF0000000000000084000000FF0000000000000000000000000000000000
      00000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000000000000FFFFFF00FFFFFF000000
      00000000000000000000000000000000000084000000FFFFFF0000000000C6C6
      C600C6C6C60084000000C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      00008400000084000000840000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF0084848400FF00
      00008400000000000000FF000000840000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000C6C6
      C600C6C6C60084000000C6C6C600C6C6C60084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00848484008400
      00008400000084000000840000008400000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484008400
      0000FF0000000000000084000000FF0000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000084000000FFFFFF0000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000084008484840084848400FFFFFF00848484000000
      00000000000084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF0084848400FF00
      00008400000000000000FF000000840000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFF
      FF00000000000000000000008400000084008484840084848400848484000000
      00000000000084000000000084008484840084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484008400
      0000FF0000000000000084000000FF000000000000000000000000000000FFFF
      FF00FFFFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF000000
      00000000000000000000000000000000000084000000FFFFFF0000000000C6C6
      C600C6C6C60084000000C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000084000000840084848400848484000000
      00000000000084000000000084008484840084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF0084848400FF00
      00008400000000000000FF000000840000000000000000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000C6C6
      C600840000008400000084000000C6C6C60084848400FFFFFF00FFFFFF000000
      0000000000000000000000000000000084000000840084848400848484000000
      00000000840000008400848484000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484008400
      0000FF0000000000000084000000FF0000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF000000
      00000000000000000000000000000000000084000000FFFFFF00000000008400
      00008400000084000000840000008400000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00848484000000840000008400848484008484
      84000000840000008400000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000C6C6
      C600C6C6C60084000000C6C6C600C6C6C60084848400FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000840000008400000084000000
      84000000840084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000000000000000000000000000FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000C6C6
      C600C6C6C60084000000C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF008484840000008400000084000000
      84008484840084000000000000000000000084848400FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084000000FFFFFF0000000000C6C6
      C600C6C6C60084000000C6C6C600C6C6C60084848400FFFFFF00000000000000
      000000000000FFFFFF0084848400848484000000840000008400000084000000
      84008484840084000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000008400000000000000C6C6
      C6008400000000000000C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000840000008400FFFFFF00FFFFFF00848484000000
      84000000840084848400848484000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      000000000000C6C6C600C6C6C600C6C6C60084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484008484
      8400000084000000840084848400000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6
      C600C6C6C600C6C6C600C6C6C600C6C6C6008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000840000008400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000840000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF00000000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000008400000084000000840000008400000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      00000000000084000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000008400000084000000840000008400000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      00008400000084000000840000000000000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF00000000000000000000000000FFFFFF0000000000FFFF
      FF0084848400FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000840000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00848484008400
      00008400000084000000840000008400000000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF00000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF000000840084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000FFFFFF000000
      00008400000000000000FFFFFF0000000000FFFFFF0000000000840000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000084000000840084848400000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000840000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000008400
      0000840000008400000000000000000000000000000084000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000084000000840084848400848484000000000000000000000084000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000084000000840084848400000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000840000008400
      0000840000008400000084000000000000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000840000008400848484008484840000008400000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000084000000840000008400000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000008400000084000000840000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000084000000840000008400000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840000008400000084000000840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000084000000840000008400000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00848484000000
      0000000000008400000000000000000000000000000084000000000000000000
      0000000000008400000000000000000000000000000084000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000848484000000840000008400000084000000840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000084000000840000008400000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000008400
      0000000000000000000000000000840000000000000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      8400000084000000840084848400000000000000840000008400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000084000000840000008400000000000000
      00000000000000000000000000000000000084848400FFFFFF00000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000008400000000000000000000000000000084000000000000000000
      0000000000008400000000000000000000000000000084000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000008400000084008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000084000000840000008400000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000840000000000000000000000
      00000000000000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084848400848484008484
      8400848484008484840084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF000000000000000000FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084008400840084000000
      0000840084000000000000000000000000000000000000000000000000008400
      8400000000008400840084008400000000000000000000000000FFFFFF00FFFF
      FF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000008400
      8400840084000000000000000000000000000000000000000000000000008400
      84008400840000000000000000008400840000000000FFFFFF00FFFFFF000000
      0000FFFFFF00FFFFFF000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000840084000000000000000000000000000000000000000000000000008400
      840000000000000000000000000084008400000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084008400840084008400
      8400840084000000000000000000000000000000000000000000000000008400
      8400000000000000000000000000840084000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008400000084000000FFFFFF00FFFFFF00000000000000
      0000FFFFFF000000000000000000000000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400840000000000000000000000
      0000840084000000000000000000000000000000000000000000000000008400
      84008400840000000000000000008400840000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084000000840000008400000084000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084008400840084008400
      8400000000000000000000000000000000000000000000000000000000008400
      840000000000840084008400840000000000000000000000000000000000FFFF
      FF00840000008400000084000000840000008400000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      8400000000000000000000000000000000000000000000000000000000000000
      000084848400FFFFFF008400000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000008400000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000840000008400000084848400FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008400000084000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008400000084000000FF00
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000FF000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000FF000000FF00
      0000FF00000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      000084000000FF000000FF000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      000084000000FF000000FF000000000000000000000084848400000000000000
      000000000000000000000000000000000000840000008400000084000000FF00
      0000FF000000FF000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000840000008400
      0000840000000000000000000000000000000000000084848400848484000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000FF000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF0000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF0000008400
      0000000000000000000000000000000000000000000000008400848484000000
      0000000000000000000000000000FF000000FF00000084000000840000008400
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000000000000000000000000000000000000000000000008400000084008484
      8400000000000000000000000000FF00000000008400FF000000840000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      0000000000000000000000000000000000000000000000008400000084008484
      84008484840000000000000000000000840000008400FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000084848400840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000084000000
      84008484840084848400000084000000840000000000FF000000FF0000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084000000000000000000
      0000000000000000000084000000840000008400000084000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000000000000000000000000000000000008400000084848400000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      8400000084000000840000008400848484000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000008400000084000000840000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      8400000084000000840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000000000000000
      0000000000000000000000000000840000008400000084000000000000000000
      0000000000000000000000000000000000008400000084000000840000000000
      0000000000000000000000000000000000000000000084000000000000000000
      0000000000000000000000000000000000000000000084848400000084000000
      8400000084000000840084848400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840084000000000000000000
      0000000000008484840084000000000000000000000084000000000000000000
      0000000000000000000000000000000000008400000000000000000000008400
      0000848484000000000000000000000000008400000084848400000000000000
      0000000000000000000000000000000000000000840000008400000084008484
      8400000000000000840000008400848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000084848400840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000008400000084008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000084000000840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF00840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000000000000000000000
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      0000FF0000000000000000000000000000000000000000000000000000000000
      000084000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF00840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000000000008484840000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008400000084000000FF00
      0000FF000000FF00000000000000000000000000000000000000FF000000FF00
      00008400000000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000FF000000FF000000FF000000000000000000000000000000FF000000FFFF
      FF00840000000000000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400848484008484
      8400000000000000000000000000000000008484840000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      000084000000FF000000FF000000000000000000000000000000FF000000FFFF
      FF00840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000008484840000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000840000008400
      0000840000000000000000000000000000000000000000000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF0000000000
      000000000000000000000000000000000000000000000000000000FFFF000000
      000000FFFF000000000000000000000000000000000000000000848484000000
      000000000000000000000000000000000000000000008484840000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FF000000FF000000FF0000008400
      0000000000000000000000000000000000008400000084000000FF000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FF0000000000
      0000000000000000000000000000000000000000000000FFFF000000000000FF
      FF000000000000FFFF0000000000000000000000000000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      0000000000000000000000000000000000008400000000000000FF0000000000
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      000084000000840000008400000084000000000000000000000000FFFF000000
      000000FFFF008400000000FFFF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      00000000000000000000000000000000000084000000FFFFFF00FF000000FF00
      0000FF000000FF000000FF000000FF000000FF000000FF000000FF0000000000
      0000840000008400000084000000000000000000000000FFFF000000000000FF
      FF008400000000FFFF0000000000000000000000000000FFFF00000000000000
      000000000000000000000000000000000000000000000000000000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000008400000000000000000000000000
      000084000000840000008400000084848400000000000000000000FFFF008400
      000000FFFF000000000000FFFF000000000000FFFF0000000000000000000000
      00000000000000000000000000000000000084848400000000008484840000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000FFFFFF008400000000000000000000000000
      0000840000000000000000000000840000000000000000FFFF000000000000FF
      FF000000000000FFFF000000000000FFFF000000000000FFFF00000000000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000008484
      840000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000000000000840000008400
      0000840000008400000084000000840000008400000000000000000000000000
      000000000000000000008484840084000000000000000000000000FFFF008484
      8400000000000000000000000000000000000000000000000000848484000000
      00000000000000000000000000000000000000FFFF0000FFFF00000000008484
      840000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000000000000000000000000
      0000000000000000000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000FFFF0000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084848400848484008484840084848400848484008484
      8400848484008484840084848400000000000000000000008400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF0000FFFF000000000000FFFF000000000000FF
      FF000000000000FFFF0084848400000000000000000000000000000084000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF000000000000FFFF000000000000FFFF000000
      000000FFFF000000000084848400000000000000000084848400000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF0000FFFF000000000000FFFF000000000000FF
      FF000000000000FFFF0084848400000000000000000000000000FFFFFF000000
      00000000840000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400000000000000000000000000FFFFFF00FFFF
      FF00000000000000840000000000FFFFFF00FFFFFF0000000000008484000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000848484000000000000FFFF000000000000FFFF00848484008484
      8400848484008484840084848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF000000000000008400000000000000000000000000008484000084
      840000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00848484000000000000FFFF000000000084848400FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000084840000848400000000000000
      0000FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00848484008484840084848400FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000848400008484000084840000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00000000000000000000848400008484000084840000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000084840000848400000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000FFFFFF00FFFF
      FF0000000000000000000000000000000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000084000000FFFFFF00840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000008400FFFFFF0000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000000000000840000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000000000000000
      000000000000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF0084000000000000000000000000008400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF0084000000000000000000000000000000008484008484
      840000848400848484000084840084000000FFFFFF0084000000840000008400
      00008400000084000000FFFFFF00840000000000000000008400000084000000
      8400FFFFFF000000000000000000000000000000000000000000000000000000
      8400FFFFFF000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000840000000000000084000000000000000000
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000848484000084
      840084848400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000008400000084000000
      8400FFFFFF00000000000000000000000000000000000000000000008400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000000000000084000000000000000000
      00008400000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF0084000000000000000000000000000000008484008484
      840000848400848484000084840084000000FFFFFF0084000000840000008400
      0000FFFFFF008400000084000000840000000000000000000000000084000000
      840000008400FFFFFF000000000000000000000000000000840000008400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000840000008400
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000000000000848484000084
      840084848400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0084000000FFFFFF00840000000000000000000000000000000000
      84000000840000008400FFFFFF00000000000000840000008400FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840000000000000084000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000FFFFFF008400
      0000840000008400000084000000000000000000000000000000008484008484
      840000848400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF008400000084000000000000000000000000000000000000000000
      00000000840000008400000084000000840000008400FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000FFFFFF008400000000000000000000000000000000000000848484000084
      8400848484000084840084848400840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      000000000000000084000000840000008400FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000840000000000000000000000000000000000000000000000008484008484
      8400008484008484840000848400848484000084840084848400008484008484
      8400008484008484840000000000000000000000000000000000000000000000
      00000000840000008400000084000000840000008400FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000084000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000000000000848484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000848484000084840000000000000000000000000000000000000000000000
      84000000840000008400FFFFFF000000000000008400FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484008484
      8400000000000000000000000000000000000000000000000000000000000000
      0000008484008484840000000000000000000000000000008400000084000000
      840000008400FFFFFF000000000000000000000000000000840000008400FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000084
      8400848484000000000000FFFF00000000000000000000FFFF00000000000084
      8400848484000084840000000000000000000000840000008400000084000000
      8400FFFFFF000000000000000000000000000000000000000000000084000000
      8400FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      0000000000000000000000000000000000000000840000008400FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      840000008400FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000848400008484000000
      0000000000000000000000000000FFFFFF000000000000848400000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF000000
      0000008484000084840000848400008484000084840000848400008484000084
      8400008484000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000848400008484000000
      0000000000000000000000000000FFFFFF000000000000848400000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF00000000000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000848400008484000000
      0000000000000000000000000000000000000000000000848400000000000084
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF000000000000848400008484000084840000848400008484000084
      8400008484000084840000848400000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000848400008484000084
      8400008484000084840000848400008484000084840000848400000000000084
      840000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000848400008484000000
      0000000000000000000000000000000000000084840000848400000000000084
      840000000000008484000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000008484000084
      8400000000000000000000000000000000000000000000000000000000000000
      000000848400008484000000000000000000000000000084840000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000848400000000000084
      840000000000008484000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000000000000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000008484000000000000000000000000000084840000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000848400000000000084
      840000000000008484000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000008484000000000000000000000000000084840000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000084
      840000000000008484000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000008484000000000000000000000000000084840000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00000000000084
      840000000000008484000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000008484000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000084
      840000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF0000000000008484000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000008484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000084840000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000500100000100010000000000800A00000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFC00FFFFFFFF
      FE008000FFFFEFFDFE000000FFFFC7FFFE000000FFFFC3FB80000000FFF7E3F7
      80000001C1F7F1E780000003C3FBF8CF80000003C7FBFC1F80010003CBFBFE3F
      80030003DCF7FC1F80070FC3FF0FF8CF807F0003FFFFE1E780FF8007FFFFC3F3
      81FFF87FFFFFC7FDFFFFFFFFFFFFFFFF8001FFFFFFFFFFFF0001FFFF0007F9FF
      0001F8070013F6CF0001F807001BF6B70001F003001BF6B70001E003001BF8B7
      0001E003001FFE8FF0CFC003001FFE3FF81F8003001BFF7FF83F88030011FE3F
      F83FF8030000FEBFF01FF803001BFC9FE10FF807001BFDDFFF87F80F001BFDDF
      FFC7FC1F0013FDDFFFFFFE7F0007FFFFFFFFFFFFFFFFFFFFFFFFFFFFC7FFFF3F
      FFFFFFFFE781FE3FFFFFFFFFD781C07F00030003A000B5000003000360006000
      380300036000400020030003600000000003000360004000000300036000A000
      26030003E000C000000300030000C000000300030000C000000300030000C000
      000300030000C000000300030000C000FFFFFFFFFFFFFFFFFFFFF0079E7FFFFF
      8003F2A76DBFF8FF8003F5570C3FF0FF8003F2A7000082018003C55780001401
      8003C407CE0008018003C007D00000018003C01FCA8080018003001FC0008001
      80035010C000800180030011C000800180035570C000800180032A76C0008001
      8003407CC0008001FFFF007DC0008001FFFFFFFFFFFFFFFFFFFFFFFFF801FFFF
      FFFFFFFFF8018003FFDFFBFFF8018003FFCFF3FFF8018003FFC7E3FFC0018003
      F003C00FC0018003F001800FC001800300000000C00F800300018000000F8003
      0003C000000F80030047E200000F8003004FF200003F8003005FFA00003F8003
      007FFE00003F8003007FFE00003FFFFF83E0FFFFFFFFFFFF83E0FC00FFFF800F
      83E0FC00C001800FA3E8FC00C001800F83E0FC00C001800FEFDF0000C001800F
      EFBF0000C001800FEF7F0000C001800FEEFF003FC001800FC5FF0001C001800F
      83E00001C001800D01E00001C001FFFD0000F801C001FFEB83E8F801C001FFE7
      C7E0F801C001FFE3EFFFF801FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      3FFD3FF83FF83FFD1FF01FFE1FFA1FFD8FF58FFE8FFA8FFDC7F9C7F8C7F9C7FE
      E3F9E3FBE3FBE3FEF1FDF1F8F1FDF1F8F8F7F8F7F8F7F8F7FC63FC63FC63FC63
      FE01FE01FE01FE01FF03FF03FF03FF03FF07FF07FF07FF07FE07FE07FE07FE07
      F80FF80FF80FF80FFC3FFC3FFC3FFC3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      F0013FF83FF83FF8F4011FFD1FFB1FFEF7F98FFD8FFD8FFE12A1C7FDC7FEC7F9
      00A9E3F9E3FAE3FE1509F1FDF1F8F1F9F7F9F8F7F8F7F8F7F7FDFC63FC63FC63
      F001FE01FE01FE01FEEFFF03FF03FF03FC47FF07FF07FF07FC47FE07FE07FE07
      FC47F80FF80FF80FFFFFFC3FFC3FFC3FFFFFFFFFFFFFFFFFFF3FFF03BFFBFE3F
      FE3FFE01800BFC1FC07FFC00800BFC1FB50000000000FE7F60007C008002FC3F
      40000000800BFC3F00000001800BFC3F40000003800BFC1FA0000087800BF20F
      C00000BF800BE107C00000BF800BE187C00000BF800BE007C00000BF800BF00F
      C00000BF800BF81FC000003F8003FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7FF
      F8FFFFFFFFFFE781F0FFFFFFFFFFD781820100030003A0001401000300036000
      0801380300036000000120030003600080010003000360008001000300036000
      800126030003E000800100030003000080010003000300008001000300030000
      80010003000300008001000300030000FFFFFFFFFFFFFFFFFFFFFFFFFFFF9E7F
      CE7FFE73FFFF6DBF9F3FFCF98F1F0C3F9F3FFCF956AF00009F3FFCF9264F8000
      9F3FFCF9162FCE003F9BF9FC060FD0009F31ECF9090FCA809F20E4F9BFEEC000
      9B3B00F9DDF6C0009B3BE4F9EDFAC000CA7BEE73F5FDC000FBFBFFFFFBFFC000
      F9F3FFFFFFFFC000FC07FFFFFFFFC000FFFFFFFFFFFFFFFF001FFFFFFFFFFFFF
      001FFFFFF3E7E4FF001FFEFFE7F3CE7F000FFE7FE7F3CE7F000FFE3FE7F3CE7F
      0007E01FE7F3CE7F0013E00FCF799F370013E007E633CE63001BE00FE413CE41
      0019E01FE773CE77001DFE3FE773CE77001DFE7FF367E4F7001FFEFFFF7FFFF7
      001FFFFFFE7F7FE7001FFFFF00FF000FFFFFFFC8FFFFFFFFFFFF80A4001F001F
      F8078020001B0004F807802000110004F003802000000004E0038020001B0004
      E003802000180004C0038020001800048003802000110004880380200003001F
      F80380200003001FF80380200003001FF80780200003001FF80F80A40001001F
      FC1FFFC80001001FFE7F00000018FFFF80018001FFFFFFFF00010001FE7F001F
      00010001FC3F001B00010001FC3F001100010001FE7F00002AA90001FFFF001B
      55550001FE7F001B638DF0CFFC3F001B4105F81FFC3F001B77DDF83FFC3F001B
      F6DFF83FFC3F001BBBBBF01FFC3F001BEEEFE10FFC3F001BBBBBFF87FC3F001B
      FFFFFFC7FE7F001FFFFFFFFFFFFF001FFFFFFFFFFFFFF3FFFEFFFDFFFFFFE0FF
      FFFFFFFFFFFFC03FC27F84FFFFFF800FFFFFFFFFFFFF0003C200840397E98001
      FFFFFFFF67E60003DE07807F766E8001CE07807F866E800307FFFFFF77E60003
      CE0180078FE98001DE018007FFEFE003FFFFFFFFFFEFF807C2008403FFFFFC0F
      FFFFFFFFFFFFFC9FFEFFFDFFFFFFFFFFBFC7BFC7FFFFFFFFDF83DF837F8FFFFF
      EF01EF01BF0707C1F700F700DE0307C1FB00FB00AE0107C1FC01FC0196010101
      FE07FE0798030001FF07FF078C0F0201FF87FF87860F02019FCFFE4FC00F8003
      3C2F0F2FC09FC1077E371FB7C1DFC1077E3F1FBF80EFE38F39BF673F08FFE38F
      83FFF07FFC7FE38FFFFFFFFFFE3FFFFFFFFFFFFFF0E1FFFFF007F8F8F0E17FFF
      F2A7FAFAF0E1BFC7F557F870F001DF83F2A7F800F001EF01C557FA10F001F700
      C407FA108843FB00C00700008CE7FC01C01F40008CE7FE07001F0A13CFFFFF07
      50105053CFFFFF8700112213CFFFFFCF5570455F47FFFFEF2A762A9F23FFFFF7
      407C401F23FFFFFF007D8FFF07FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC003FFF
      FFFFFFFFF8001FFF0055540100A88FFF0055540171548001FFFFFFFF70A88001
      FFE7CFFF70008001FFFBBFFF72818001FFFD7FFF7143800187FD7FC370038001
      8FFD7FE3700380018FFD7FE370038001B7FBBFDB00038001F9E7CF3F00038001
      FE1FF0FF00038001FFFFFFFFFFFF8001FFFFFFFFFFFFFFFFF9FFFC01FE00FFF3
      F6CFFC01C000CFFFF6B7FC01800087E7F6B70001800087CFF8B700018000C38F
      FE8F00018000E11FFE3F00018001F03FFF7F00038001F87FFE3F00078001F03F
      FEBF000F8001E13FFC9F00FF87E1838FFDDF01FF800107C7FDDF03FFC0031FE3
      FDDFFFFFFC3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF800FC001801F
      C00780078031001FC007800380310007C007800180310007C007800080010001
      C007800080010001C007800F80010001C007800F8FF10001C007800F8FF10001
      C007C7F88FF10001C007FFFC8FF10001C00FFFBA8FF1C001C01FFFC78FF5C001
      C03FFFFF8001F001FFFFFFFFFFFFF00100000000000000000000000000000000
      000000000000}
  end
  object JvDockVCStyle1: TJvDockVCStyle
    ConjoinServerOption.GrabbersSize = 15
    ConjoinServerOption.SplitterWidth = 4
    Left = 136
    Top = 104
  end
  object OpenDialog1: TOpenDialog
    Filter = 'C++ File(*.c;*cpp;*h)|*.c;*cpp;*h|All File(*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofEnableSizing]
    Left = 168
    Top = 104
  end
  object lbDockServer1: TJvDockServer
    LeftSplitterStyle.Cursor = crHSplit
    LeftSplitterStyle.ParentColor = False
    LeftSplitterStyle.Size = 4
    TopSplitterStyle.Cursor = crVSplit
    TopSplitterStyle.ParentColor = False
    TopSplitterStyle.Size = 4
    RightSplitterStyle.Cursor = crHSplit
    RightSplitterStyle.ParentColor = False
    RightSplitterStyle.Size = 4
    BottomSplitterStyle.Cursor = crVSplit
    BottomSplitterStyle.ParentColor = False
    BottomSplitterStyle.Size = 4
    OnGetClientAlignSize = lbDockServer1GetClientAlignSize
    DockStyle = JvDockVCStyle1
    Left = 104
    Top = 104
  end
  object ViewPopupMenu: TPopupMenu
    Left = 200
    Top = 104
    object Output_PopupItem: TMenuItem
      Action = View_OutPut_Action
    end
    object Watch_PopupItem: TMenuItem
      Action = View_Watch_Action
    end
    object Variables_PopupItem: TMenuItem
      Action = View_Variables_Action
      Caption = '&Variables'
    end
    object Registers_PopupItem: TMenuItem
      Action = View_Registers_Action
    end
    object Memory_PopupItem: TMenuItem
      Action = View_Memory_Action
    end
    object CallStack_PopupItem: TMenuItem
      Action = View_CallStack_Action
    end
    object Workspace_PopupItem: TMenuItem
      Action = View_Workspace_Action
    end
    object N36: TMenuItem
      Caption = '-'
    end
    object Standard1: TMenuItem
      Action = View_Standard_Toolbar_Action
    end
    object BuildMinibar1: TMenuItem
      Action = View_Build_Minibar_Toolbar_Action
    end
    object Edit2: TMenuItem
      Action = View_Edit_Toolbar_Action
    end
    object Debug2: TMenuItem
      Action = View_Debug_Toolbar_Action
    end
    object N37: TMenuItem
      Caption = '-'
    end
    object BuildMinibar2: TMenuItem
      Action = View_Customize_Toolbar_Action
    end
  end
end
