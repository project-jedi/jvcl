object MSDN2002: TMSDN2002
  Left = 239
  Top = 193
  Width = 664
  Height = 488
  Caption = 'MSDN Library - April 2002'
  Color = clGray
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Courier New'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000444440000000004444444440000004C4CFF7444400000C4
    C4FF744440000C4C4CCCCC4444000CC4C4FF74C444000CCC4CFF7C4C44000CCC
    CCCFF7C444000CCCC77CFF4C44000CCCCFFCFFC4C40000CCC7FFF74C400000CC
    CC777CCC4000000CCCCCCCC4000000000CCCCC0000000000000000000000FFFF
    0000F83F0000E00F0000C0070000C00700008003000080030000800300008003
    00008003000080030000C0070000C0070000E00F0000F83F0000FFFF0000}
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object ControlBar1: TControlBar
    Left = 0
    Top = 0
    Width = 656
    Height = 26
    Align = alTop
    AutoSize = True
    BevelEdges = []
    Color = clBtnFace
    ParentColor = False
    TabOrder = 0
    object ToolBar1: TToolBar
      Left = 11
      Top = 2
      Width = 643
      Height = 22
      Align = alClient
      AutoSize = True
      Caption = 'ToolBar1'
      EdgeBorders = []
      Flat = True
      Images = ImageList1
      List = True
      TabOrder = 0
      object ToolButton1: TToolButton
        Left = 0
        Top = 0
        Action = View_Web_Browser_Web_Navigate_Back_Action
      end
      object ToolButton2: TToolButton
        Left = 23
        Top = 0
        Action = View_Web_Browser_Web_Navigate_Forward_Action
      end
      object ToolButton3: TToolButton
        Left = 46
        Top = 0
        Action = View_Web_Browser_Stop_Browser_Action
      end
      object ToolButton4: TToolButton
        Left = 69
        Top = 0
        Action = View_Web_Browser_Refresh_Browser_Action
      end
      object ToolButton5: TToolButton
        Left = 92
        Top = 0
        Action = View_Web_Browser_Home_Action
      end
      object ToolButton6: TToolButton
        Left = 115
        Top = 0
        Width = 8
        Caption = 'ToolButton6'
        ImageIndex = 5
        Style = tbsSeparator
      end
      object ToolButton7: TToolButton
        Left = 123
        Top = 0
        Action = View_Web_Browser_Search_Action
      end
      object ToolButton8: TToolButton
        Left = 146
        Top = 0
        Action = Help_Favorites_Action
      end
      object ToolButton9: TToolButton
        Left = 169
        Top = 0
        Action = Help_Add_to_Favorites_Action
      end
      object ToolButton10: TToolButton
        Left = 192
        Top = 0
        Width = 8
        Caption = 'ToolButton10'
        ImageIndex = 8
        Style = tbsSeparator
      end
      object URLComboBox: TComboBox
        Left = 200
        Top = 0
        Width = 225
        Height = 23
        ImeName = #215#207#185#226#198#180#210#244#202#228#200#235#183#168
        ItemHeight = 15
        TabOrder = 0
      end
      object ToolButton11: TToolButton
        Left = 425
        Top = 0
        Width = 8
        Caption = 'ToolButton11'
        ImageIndex = 9
        Style = tbsSeparator
      end
      object ToolButton12: TToolButton
        Left = 433
        Top = 0
        Action = Window_New_Window_Action
      end
      object ToolButton17: TToolButton
        Left = 456
        Top = 0
        Width = 8
        Caption = 'ToolButton17'
        ImageIndex = 14
        Style = tbsSeparator
      end
      object ToolButton13: TToolButton
        Left = 464
        Top = 0
        Action = Help_Sync_Contents_Action
      end
      object ToolButton14: TToolButton
        Left = 487
        Top = 0
        Action = Help_Previous_topic_Action
      end
      object ToolButton15: TToolButton
        Left = 510
        Top = 0
        Action = Help_Next_topic_Action
      end
      object ToolButton18: TToolButton
        Left = 533
        Top = 0
        Width = 8
        Caption = 'ToolButton18'
        ImageIndex = 14
        Style = tbsSeparator
      end
      object ToolButton16: TToolButton
        Left = 541
        Top = 0
        Action = View_Text_Size_Action
      end
      object ToolButton19: TToolButton
        Left = 564
        Top = 0
        Width = 8
        Caption = 'ToolButton19'
        ImageIndex = 14
        Style = tbsSeparator
      end
      object ToolButton20: TToolButton
        Left = 572
        Top = 0
        Action = Edit_Cut_Action
      end
      object ToolButton21: TToolButton
        Left = 595
        Top = 0
        Action = Edit_Copy_Action
      end
      object ToolButton22: TToolButton
        Left = 618
        Top = 0
        Action = Edit_Paste_Action
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 420
    Width = 656
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object ActionList1: TActionList
    Images = ImageList1
    Left = 48
    Top = 32
    object File_File_Action: TAction
      Category = 'File_Category'
      Caption = '&File'
      OnExecute = File_File_ActionExecute
    end
    object File_Print_Action: TAction
      Category = 'File_Category'
      Caption = '&Print'
      ImageIndex = 0
      ShortCut = 16464
      OnExecute = File_Print_ActionExecute
    end
    object View_View_Action: TAction
      Category = 'View_Category'
      Caption = '&View'
      OnExecute = File_File_ActionExecute
    end
    object View_Web_Browser_Action: TAction
      Category = 'View_Category'
      Caption = 'Web Browser'
      OnExecute = File_File_ActionExecute
    end
    object View_Web_Browser_Show_Web_Browser_Action: TAction
      Category = 'View_Category'
      Caption = 'Show Web &Browser'
      ImageIndex = 8
      ShortCut = 49234
      OnExecute = File_Print_ActionExecute
    end
    object File_Exit_Action: TAction
      Category = 'File_Category'
      Caption = 'E&xit'
      OnExecute = File_Exit_ActionExecute
    end
    object Edit_Edit_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Edit'
      OnExecute = File_File_ActionExecute
    end
    object Edit_Undo_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Undo'
      ImageIndex = 1
      ShortCut = 16474
      OnExecute = File_Print_ActionExecute
    end
    object Edit_Redo_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Redo'
      ImageIndex = 2
      ShortCut = 16473
      OnExecute = File_Print_ActionExecute
    end
    object Edit_Cut_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Cu&t'
      ImageIndex = 3
      ShortCut = 16472
      OnExecute = File_Print_ActionExecute
    end
    object Edit_Copy_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Cop&y'
      ImageIndex = 4
      ShortCut = 16451
      OnExecute = File_Print_ActionExecute
    end
    object Edit_Paste_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Paste'
      ImageIndex = 5
      ShortCut = 16470
      OnExecute = File_Print_ActionExecute
    end
    object Edit_Delete_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Delete'
      ImageIndex = 6
      ShortCut = 46
      OnExecute = File_Print_ActionExecute
    end
    object Edit_Select_All_Action: TAction
      Category = 'Edit_Category'
      Caption = 'Select &All'
      ShortCut = 16449
      OnExecute = File_Print_ActionExecute
    end
    object Edit_Find_in_this_Topic_Action: TAction
      Category = 'Edit_Category'
      Caption = '&Find in this Topic'
      ImageIndex = 7
      ShortCut = 16454
      OnExecute = File_Print_ActionExecute
    end
    object View_Web_Browser_Web_Navigate_Back_Action: TAction
      Category = 'View_Category'
      Caption = 'Web Navigate &Back'
      ImageIndex = 9
      ShortCut = 32805
      OnExecute = File_Print_ActionExecute
    end
    object View_Web_Browser_Web_Navigate_Forward_Action: TAction
      Category = 'View_Category'
      Caption = 'Web Navigate &Forward'
      ImageIndex = 10
      ShortCut = 32807
      OnExecute = File_Print_ActionExecute
    end
    object View_Web_Browser_Stop_Browser_Action: TAction
      Category = 'View_Category'
      Caption = 'Stop Browser'
      ImageIndex = 11
      OnExecute = File_Print_ActionExecute
    end
    object View_Web_Browser_Refresh_Browser_Action: TAction
      Category = 'View_Category'
      Caption = 'Refresh Browser'
      ImageIndex = 12
      OnExecute = File_Print_ActionExecute
    end
    object View_Web_Browser_Home_Action: TAction
      Category = 'View_Category'
      Caption = '&Home'
      ImageIndex = 13
      OnExecute = File_Print_ActionExecute
    end
    object View_Web_Browser_Search_Action: TAction
      Category = 'View_Category'
      Caption = 'S&earch'
      ImageIndex = 14
      OnExecute = File_Print_ActionExecute
    end
    object View_Navigation_Action: TAction
      Category = 'View_Category'
      Caption = 'Navigation'
      OnExecute = File_File_ActionExecute
    end
    object View_Toolbars_Action: TAction
      Category = 'View_Category'
      Caption = 'Toolbars'
      OnExecute = File_File_ActionExecute
    end
    object View_Toolbars_Full_Screen_Action: TAction
      Category = 'View_Category'
      Caption = 'Full Screen'
      OnExecute = File_Print_ActionExecute
    end
    object View_Toolbars_Standard_Action: TAction
      Category = 'View_Category'
      Caption = 'Standard'
      OnExecute = File_Print_ActionExecute
    end
    object View_Toolbars_Customize_Action: TAction
      Category = 'View_Category'
      Caption = 'Customize...'
      OnExecute = File_Print_ActionExecute
    end
    object View_Full_Screen_Action: TAction
      Category = 'View_Category'
      Caption = 'F&ull Screen'
      ImageIndex = 15
      ShortCut = 40973
      OnExecute = File_Print_ActionExecute
    end
    object View_Text_Size_Action: TAction
      Category = 'View_Category'
      Caption = 'Text Si&ze'
      ImageIndex = 16
      OnExecute = File_File_ActionExecute
    end
    object View_Text_Size_Largest_Action: TAction
      Category = 'View_Category'
      Caption = 'Lar&gest'
      OnExecute = File_Print_ActionExecute
    end
    object View_Text_Size_Larger_Action: TAction
      Category = 'View_Category'
      Caption = 'Larger'
      OnExecute = File_Print_ActionExecute
    end
    object View_Text_Size_Medium_Action: TAction
      Category = 'View_Category'
      Caption = '&Medium'
      OnExecute = File_Print_ActionExecute
    end
    object View_Text_Size_Smaller_Action: TAction
      Category = 'View_Category'
      Caption = '&Smaller'
      OnExecute = File_Print_ActionExecute
    end
    object View_Text_Size_Smallest_Action: TAction
      Category = 'View_Category'
      Caption = 'Smalles&t'
      OnExecute = File_Print_ActionExecute
    end
    object View_View_Source_Action: TAction
      Category = 'View_Category'
      Caption = '&View Source'
      OnExecute = File_Print_ActionExecute
    end
    object Tools_Tools_Action: TAction
      Category = 'Tools_Category'
      Caption = '&Tools'
      OnExecute = File_File_ActionExecute
    end
    object Tools_Customize_Action: TAction
      Category = 'Tools_Category'
      Caption = 'Customize...'
      OnExecute = File_Print_ActionExecute
    end
    object Tools_Options_Action: TAction
      Category = 'Tools_Category'
      Caption = 'Options...'
      OnExecute = File_Print_ActionExecute
    end
    object Window_Window_Action: TAction
      Category = 'Window_Category'
      Caption = '&Window'
      OnExecute = File_File_ActionExecute
    end
    object Window_Split_Action: TAction
      Category = 'Window_Category'
      Caption = 'S&plit'
      ImageIndex = 18
      OnExecute = File_Print_ActionExecute
    end
    object Window_New_Window_Action: TAction
      Category = 'Window_Category'
      Caption = '&New Window'
      ImageIndex = 17
      OnExecute = File_Print_ActionExecute
    end
    object Window_Dockable_Action: TAction
      Category = 'Window_Category'
      Caption = 'Doc&kable'
      ImageIndex = 19
      OnExecute = File_Print_ActionExecute
    end
    object Window_Hide_Action: TAction
      Category = 'Window_Category'
      Caption = '&Hide'
      OnExecute = File_Print_ActionExecute
    end
    object Window_Floating_Action: TAction
      Category = 'Window_Category'
      Caption = '&Floating'
      OnExecute = File_Print_ActionExecute
    end
    object Window_Auto_Hide_Action: TAction
      Category = 'Window_Category'
      Caption = '&Auto Hide'
      OnExecute = File_Print_ActionExecute
    end
    object Window_Close_All_Document_Action: TAction
      Category = 'Window_Category'
      Caption = 'C&lose All Document'
      OnExecute = File_Print_ActionExecute
    end
    object Window_Windows_Action: TAction
      Category = 'Window_Category'
      Caption = 'Windows...'
      OnExecute = File_Print_ActionExecute
    end
    object Help_Help_Action: TAction
      Category = 'Help_Category'
      Caption = '&Help'
      OnExecute = File_File_ActionExecute
    end
    object Help_Contents_Action: TAction
      Category = 'Help_Category'
      Caption = '&Contents...'
      ImageIndex = 20
      ShortCut = 49264
      OnExecute = Help_Contents_ActionExecute
    end
    object Help_Index_Action: TAction
      Category = 'Help_Category'
      Caption = '&Index...'
      ImageIndex = 21
      ShortCut = 49265
      OnExecute = Help_Index_ActionExecute
    end
    object Help_Search_Action: TAction
      Category = 'Help_Category'
      Caption = '&Search...'
      ImageIndex = 22
      ShortCut = 49266
      OnExecute = Help_Search_ActionExecute
    end
    object Help_Favorites_Action: TAction
      Category = 'Help_Category'
      Caption = 'Fa&vorites'
      ImageIndex = 23
      ShortCut = 49222
      OnExecute = Help_Favorites_ActionExecute
    end
    object Help_Add_to_Favorites_Action: TAction
      Category = 'Help_Category'
      Caption = 'Add to Favorites'
      ImageIndex = 24
      OnExecute = File_Print_ActionExecute
    end
    object Help_Index_results_Action: TAction
      Category = 'Help_Category'
      Caption = 'Inde&x results...'
      ImageIndex = 25
      ShortCut = 41073
      OnExecute = Help_Index_results_ActionExecute
    end
    object Help_Search_results_Action: TAction
      Category = 'Help_Category'
      Caption = 'Search &results...'
      ImageIndex = 26
      ShortCut = 41074
      OnExecute = Help_Search_results_ActionExecute
    end
    object Help_Edit_Filters_Action: TAction
      Category = 'Help_Category'
      Caption = 'Edit &Filters...'
      OnExecute = File_Print_ActionExecute
    end
    object Help_Previous_topic_Action: TAction
      Category = 'Help_Category'
      Caption = '&Previous topic'
      ImageIndex = 27
      ShortCut = 32806
      OnExecute = File_Print_ActionExecute
    end
    object Help_Next_topic_Action: TAction
      Category = 'Help_Category'
      Caption = '&Next topic'
      ImageIndex = 28
      ShortCut = 32808
      OnExecute = File_Print_ActionExecute
    end
    object Help_Sync_Contents_Action: TAction
      Category = 'Help_Category'
      Caption = 'S&ync Contents'
      ImageIndex = 29
      OnExecute = File_Print_ActionExecute
    end
    object Help_Technical_Support_Action: TAction
      Category = 'Help_Category'
      Caption = '&Technical Support'
      ImageIndex = 30
      OnExecute = File_Print_ActionExecute
    end
    object Help_Help_on_Help_Action: TAction
      Category = 'Help_Category'
      Caption = 'H&elp on Help'
      OnExecute = File_Print_ActionExecute
    end
    object Help_About_Action: TAction
      Category = 'Help_Category'
      Caption = '&About...'
      OnExecute = File_Print_ActionExecute
    end
  end
  object MainMenu1: TMainMenu
    Images = ImageList1
    Left = 8
    Top = 32
    object File1: TMenuItem
      Action = File_File_Action
      object Print1: TMenuItem
        Action = File_Print_Action
      end
      object Exit1: TMenuItem
        Action = File_Exit_Action
      end
    end
    object Edit1: TMenuItem
      Action = Edit_Edit_Action
      object Undo1: TMenuItem
        Action = Edit_Undo_Action
      end
      object Redo1: TMenuItem
        Action = Edit_Redo_Action
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Cut1: TMenuItem
        Action = Edit_Cut_Action
      end
      object Copy1: TMenuItem
        Action = Edit_Copy_Action
      end
      object Paste1: TMenuItem
        Action = Edit_Paste_Action
      end
      object Delete1: TMenuItem
        Action = Edit_Delete_Action
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object SelectAll1: TMenuItem
        Action = Edit_Select_All_Action
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object FindinthisTopic1: TMenuItem
        Action = Edit_Find_in_this_Topic_Action
      end
    end
    object ViewSource1: TMenuItem
      Action = View_View_Action
      object WebBrowser1: TMenuItem
        Action = View_Web_Browser_Action
        object ShowWebBrowser1: TMenuItem
          Action = View_Web_Browser_Show_Web_Browser_Action
        end
        object N4: TMenuItem
          Caption = '-'
        end
        object WebNavigateBack1: TMenuItem
          Action = View_Web_Browser_Web_Navigate_Back_Action
        end
        object WebNavigateForward1: TMenuItem
          Action = View_Web_Browser_Web_Navigate_Forward_Action
        end
        object N5: TMenuItem
          Caption = '-'
        end
        object Home1: TMenuItem
          Action = View_Web_Browser_Home_Action
        end
        object Search1: TMenuItem
          Action = View_Web_Browser_Search_Action
        end
      end
      object Navigation1: TMenuItem
        Action = View_Navigation_Action
        object Contents1: TMenuItem
          Action = Help_Contents_Action
        end
        object Index1: TMenuItem
          Action = Help_Index_Action
        end
        object Search2: TMenuItem
          Action = Help_Search_Action
        end
        object Favorites1: TMenuItem
          Action = Help_Favorites_Action
        end
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object Toolbars1: TMenuItem
        Action = View_Toolbars_Action
        object FullScreen1: TMenuItem
          Action = View_Toolbars_Full_Screen_Action
        end
        object Standard1: TMenuItem
          Action = View_Toolbars_Standard_Action
        end
        object N7: TMenuItem
          Caption = '-'
        end
        object Customize1: TMenuItem
          Action = View_Toolbars_Customize_Action
        end
      end
      object FullScreen2: TMenuItem
        Action = View_Full_Screen_Action
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object ViewTextSizeAction1: TMenuItem
        Action = View_Text_Size_Action
        object Largest1: TMenuItem
          Action = View_Text_Size_Largest_Action
        end
        object Larger1: TMenuItem
          Action = View_Text_Size_Larger_Action
        end
        object Medium1: TMenuItem
          Action = View_Text_Size_Medium_Action
        end
        object Smaller1: TMenuItem
          Action = View_Text_Size_Smaller_Action
        end
        object Smallest1: TMenuItem
          Action = View_Text_Size_Smallest_Action
        end
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object ViewSource2: TMenuItem
        Action = View_View_Source_Action
      end
    end
    object Tools1: TMenuItem
      Action = Tools_Tools_Action
      object Customize2: TMenuItem
        Action = Tools_Customize_Action
      end
      object Options1: TMenuItem
        Action = Tools_Options_Action
      end
    end
    object Window1: TMenuItem
      Action = Window_Window_Action
      object Split1: TMenuItem
        Action = Window_Split_Action
      end
      object NewWindow1: TMenuItem
        Action = Window_New_Window_Action
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Dockable1: TMenuItem
        Action = Window_Dockable_Action
      end
      object Hide1: TMenuItem
        Action = Window_Hide_Action
      end
      object Floating1: TMenuItem
        Action = Window_Floating_Action
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object AutoHide1: TMenuItem
        Action = Window_Auto_Hide_Action
      end
      object CloseAllDocument1: TMenuItem
        Action = Window_Close_All_Document_Action
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object Windows1: TMenuItem
        Action = Window_Windows_Action
      end
    end
    object Help1: TMenuItem
      Action = Help_Help_Action
      object Contents2: TMenuItem
        Action = Help_Contents_Action
      end
      object Index2: TMenuItem
        Action = Help_Index_Action
      end
      object Search3: TMenuItem
        Action = Help_Search_Action
      end
      object Favorites2: TMenuItem
        Action = Help_Favorites_Action
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object Indexresults1: TMenuItem
        Action = Help_Index_results_Action
      end
      object Searchresults1: TMenuItem
        Action = Help_Search_results_Action
      end
      object N14: TMenuItem
        Caption = '-'
      end
      object EditFilters1: TMenuItem
        Action = Help_Edit_Filters_Action
      end
      object N15: TMenuItem
        Caption = '-'
      end
      object Previoustopic1: TMenuItem
        Action = Help_Previous_topic_Action
      end
      object Nexttopic1: TMenuItem
        Action = Help_Next_topic_Action
      end
      object SyncContents1: TMenuItem
        Action = Help_Sync_Contents_Action
      end
      object N16: TMenuItem
        Caption = '-'
      end
      object TechnicalSupport1: TMenuItem
        Action = Help_Technical_Support_Action
      end
      object N17: TMenuItem
        Caption = '-'
      end
      object HelponHelp1: TMenuItem
        Action = Help_Help_on_Help_Action
      end
      object About1: TMenuItem
        Action = Help_About_Action
      end
    end
  end
  object ImageList1: TImageList
    Left = 88
    Top = 32
    Bitmap = {
      494C01011F002200040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000009000000001002000000000000090
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      000000000000FF00000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      000000000000FF0000000000000000000000000000008484840000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF0000000000000000000000000000000000000000000000000000FFFF
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      000000000000FF0000000000000000000000000000008484840000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FFFF
      0000FFFF000000000000000000000000000000000000FF000000000000008400
      000000000000FF00000000000000000000000000000084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000000000000000000000000000FF000000000000000000
      000000000000000000000000000000000000000000008484840000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000000000000000000000000000FF000000000000000000
      000000000000000000000000000000000000000000008484840000FFFF0000FF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FFFF
      0000FFFF000000000000000000000000000000000000FF000000000000000000
      00000000000000000000000000008484840000000000000000008484840000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFF0000000000000000000000000000000000000000000000000000FFFF
      00000000000000000000000000000000000000000000FF000000000000000000
      00000000000000000000000000008484840000FFFF0000FFFF00000000008484
      840000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000000000008400
      000000000000FF000000000000008484840000FFFF0000FFFF00000000000000
      000000FFFF0000FFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FF000000FF000000FF00
      000000000000FF00000000000000000000008484840000FFFF0000FFFF0000FF
      FF0000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FF000000FF00
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FF00
      000000000000FF00000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084008400000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400840084008400840000000000840084000000000000000000000000000000
      000084000000FFFF000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      000000000000000000008484840000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000084848400840084008400
      84000000000000000000FFFFFF00FFFFFF008400840000000000000000008400
      0000FFFF00008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF008400000000FFFF008400000000FF
      FF008400000000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF0000FFFF00FFFFFF00840000000000
      000000FFFF00FFFFFF0000000000000000000000000084008400000000000000
      0000000000008400840000000000848484000000000000000000848484000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000FFFF00FFFFFF0000FFFF0084000000840000008400
      000000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      000000000000000000008484840000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF0000000000000000000000000084008400000000008400
      840084008400FF00FF008484840000000000FFFF000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000FFFF00FFFFFF0000FFFF00840000008400000000FFFF008400
      00008400000000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF0000FFFF00FFFFFF00840000000000
      000000FFFF00FFFFFF000000000000000000000000008400840084008400FF00
      FF00FF00FF0084848400FFFFFF00FFFF000000000000FFFF0000000000000000
      000000FFFF0000FFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF0000000000FFFFFF0000FFFF00FFFFFF0000FFFF0084000000840000008400
      000000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      00000000000000FFFF0000000000000000000000000084008400FF00FF00FF00
      FF00FF00FF0084848400FFFFFF00FFFFFF00FFFF000000000000FFFF00000000
      000000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF00FFFFFF0000FFFF008400000000FFFF008400000000FF
      FF008400000000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF00FFFFFF0000000000FFFFFF00FFFF
      FF0084000000FFFFFF000000000000000000000000000000000084008400FF00
      FF00FF00FF0084848400FFFFFF00FFFFFF00FFFFFF00FFFF0000000000000000
      0000FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF008400
      000000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000084848400FFFFFF0000FFFF008400000000000000FFFF
      FF008400000000FFFF0000000000000000000000000000000000000000008400
      8400FF00FF00FF00FF0084848400FFFFFF00FFFFFF00FFFFFF00848484000000
      0000FFFFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0084840000FF00
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400FFFFFF00FFFFFF00FFFFFF00840000008400
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000084008400FF00FF00FF00FF0084848400848484008484840000000000FFFF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00848400008484
      8400008400000000000000FFFF00FFFFFF0000FFFF00FFFFFF00000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008484840000FFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0000FFFF008484840084848400000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF0084008400FF00FF00FF00FF00000000008484840000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      000000000000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF00000000
      00000000000000000000000000000000000000000000FFFFFF0084840000FFFF
      FF00848484008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084848400848484008484840084848400848484008484
      8400848484008484840000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF008400840084008400FFFFFF0084848400FFFFFF0000FF
      FF00FFFFFF0000FFFF0084848400000000000000000000000000000000000000
      00000000000000000000FFFF0000FFFF0000FFFF0000FFFF0000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF008484
      0000848400008484000000FFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0084848400848484008484
      8400848484008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000084848400FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF0000FFFF00FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400840084008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400840084008400008484008400840000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      8400840084000000000000000000840084000000000000000000000000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      84008400840000848400FFFFFF0000000000FFFFFF0084008400000000000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000084008400840084000000
      0000000000000000000000000000FFFFFF008400840000000000000000000000
      00000000000084000000FFFF0000840000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000084008400840084000084
      8400FFFFFF00FFFFFF00000000000000000000000000FFFFFF00840084000000
      000000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FF000000FF000000FFFFFF0000FFFF00FFFF
      FF00000000000000000000000000000000008400840000000000000000000000
      000000000000840084000000000000000000FFFFFF0084008400000000000000
      000084000000FFFF00008400000084000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0084000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000008400840000848400FFFFFF00FFFF
      FF00000000000000000084848400840084008484840000000000FFFFFF008400
      840000000000000000000000000000000000000000000000000084848400FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000008400840000000000000000008400
      840084008400FF00FF00FF00FF00000000008484840000000000000000008484
      8400000000008400000084000000000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF008400000000FFFF008400000000FFFF008400000000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000008400840000000000000000000000
      0000848484008400840084008400FF00FF00000000008484840000000000FFFF
      FF0084008400000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFF
      FF0000000000000000000000000000000000840084008400840084008400FF00
      FF00FF00FF00FF00FF00FF00FF008484840000000000FFFF0000000000000000
      000084848400848484000000000000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF0084000000840000008400000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000008400840000000000848484008400
      840084008400FF00FF00FF00FF00FF00FF008400840000000000848484000000
      0000FFFFFF00840084000000000000000000000000000000000084848400FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FFFF
      FF000000000000000000000000000000000084008400FF00FF00FF00FF00FF00
      FF00FF00FF00FF00FF0084848400FFFFFF00FFFF000000000000FFFF00000000
      00000000000000FFFF0000FFFF00000000000000000000000000FFFFFF0000FF
      FF00FFFFFF00840000008400000084000000FFFFFF0084000000840000008400
      0000FFFFFF0000FFFF00FFFFFF0000000000840084008400840084008400FF00
      FF00FF00FF008400840084008400FF00FF00FF00FF0084008400000000008484
      840000000000FFFFFF008400840000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FF000000FF00000000FFFF00FFFF
      FF00000000000000000000000000000000000000000084008400FF00FF00FF00
      FF00FF00FF00FF00FF0084848400FFFFFF00FFFFFF00FFFF000000000000FFFF
      00000000000000FFFF0000FFFF0000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF0084000000840000008400000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000008400840000000000FF00FF00FF00
      FF00FF00FF0000FFFF00000000000084840084008400FF00FF00840084000000
      000084848400000000008400840000000000000000000000000084848400FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FF000000FF000000FFFF
      FF0000000000000000000000000000000000000000000000000084008400FF00
      FF00FF00FF00FF00FF0084848400FFFFFF00FFFFFF00FFFFFF00FFFF00000000
      00000000000000FFFF0000FFFF00000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF008400000000FFFF008400000000FFFF008400000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000000000000000008400840000000000FF00
      FF00FF00FF00FF00FF00FF00FF0000FFFF0000FFFF0000000000008484008400
      840000000000848484008400840000000000000000000000000084848400FFFF
      FF00FFFFFF00FF000000FF000000FFFFFF00FFFFFF00FF000000FF000000FFFF
      FF00000000000000000000000000000000000000000000000000000000008400
      8400FF00FF00FF00FF00FF00FF0084848400FFFFFF00FFFFFF00FFFFFF008484
      84000000000000FFFF0000FFFF0000000000000000000000000000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00FFFFFF0084000000FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000840084000000
      0000FF00FF00FF00FF00FF00FF00FF00FF00FF00FF000000000000FFFF008400
      840084008400000000008400840000000000000000000000000084848400FFFF
      FF0000FFFF00FF000000FF000000FFFFFF0000FFFF00FF000000FF000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      000084008400FF00FF0084008400840084008484840084848400848484000000
      0000FFFFFF0000FFFF0000FFFF00000000000000000000000000FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF00000000000000000000000000000000008400
      840000000000FF00FF00FF00FF0000FFFF0000FFFF0000FFFF00008484008400
      8400FF00FF00840084000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF00FF000000FF000000FF000000FF00000000FFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000084008400848484008484840000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF00FFFFFF0000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00008400840000000000FF00FF00FF00FF00008484000084840084008400FF00
      FF0084008400840084000000000000000000000000000000000084848400FFFF
      FF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084848400FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF0000FFFF00FFFFFF000000000000000000000000000000000000FF
      FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400840000000000FF00FF00FF00FF00FF00FF00FF00FF008400
      840000000000000000000000000000000000000000000000000084848400FFFF
      FF00FFFFFF00FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000FFFF008484
      8400848484000000000000000000000000000000000000000000000000000000
      00000000000000000000000000008484840000FFFF00FFFFFF0000FFFF00FFFF
      FF0000FFFF008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000008400840000000000FF00FF0084008400000000000000
      0000000000000000000000000000000000000000000000000000848484008484
      8400848484008484840084848400848484008484840084848400848484008484
      8400000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000084848400FFFFFF0000FFFF00FFFFFF0000FF
      FF00FFFFFF008484840084848400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000840084000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400848484008484
      8400848484008484840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      840000000000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000000000000000000000000000FFFF0000FFFF0000FFFF
      0000FFFF0000FFFF0000FFFF0000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      000000000000000000000000000000000000000000000000000000000000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      840000848400008484000000000000000000000000000000000000000000FFFF
      000000000000000000000000000000000000000000000000000000000000FFFF
      0000FFFF00000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      0000FFFF00000000000000000000000000000000000000000000FFFF0000FFFF
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      000000000000FFFF0000000000000000000000000000FFFF0000FFFF00000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFF
      FF0000000000FFFFFF0000000000000000000000000084000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF0000000000FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00840000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      000000000000FFFF0000000000000000000000000000FFFF0000FFFF00000000
      00000000000000000000000000000000000000FFFF0000000000000000008484
      8400FFFFFF0000FFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF000000
      0000FFFFFF0000000000FFFFFF00000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      00000000000000000000FFFF000000000000FFFF0000FFFF0000000000000000
      00000000000000000000000000000000000084848400FFFFFF00000000008484
      840000FFFF00FFFFFF00FFFFFF00FFFFFF0000000000FFFFFF00FFFFFF00FFFF
      FF0000000000FFFFFF0000000000000000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000084
      8400008484000084840000848400008484000084840000848400008484000084
      8400008484000084840000000000000000000000000000000000000000000000
      00000000000000000000FFFF000000000000FFFF0000FFFF0000000000000000
      000000000000000000000000000000000000000000000000000000FFFF008484
      8400840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000084000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      000000000000000000000000000000000000848484008484840084848400FFFF
      FF0000FFFF0000FFFF0084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFF0000FFFF000000000000000000000000
      00000000000000000000000000000000000000FFFF00FFFFFF008484840000FF
      FF00FFFFFF008484840000FFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000FFFFFF00FFFF
      FF00840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000008484840000FFFF008484
      840000FFFF00000000008484840000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000000000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008484840000FFFF00000000008484
      8400FFFFFF000000000000000000848484000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000FFFF0000000000000000008484
      840000FFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      000084000000FF00000084000000FF00000084000000FF000000000000000000
      0000848484000000FF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000008400000084
      00000084000000000000FF00000084000000FF00000084000000840000008484
      84000000FF000000FF0000008400000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000084000000840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000000000008400000000000000FF
      000000840000008400000000000084848400000000000000000084848400FFFF
      FF000000FF000000840000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000084000000840000008400000084000000840000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000000000FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000840000000000000084
      000000FF0000008400008484840000000000FFFF000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF000084000000840000FFFFFF00FFFFFF000084
      0000FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000000000000FFFF0000FFFF0000000000FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000840000FFFFFF000000000000FF
      00000084000084848400FFFFFF00FFFF000000000000FFFF0000000000000000
      000000840000008400000000000000000000000000000000000000000000FFFF
      0000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000840000FFFFFF00FFFFFF000084
      0000FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF0000000000000000000000000000840000FFFFFF00000000000084
      0000FF00000084848400FFFFFF00FFFFFF00FFFF000000000000FFFF00000000
      000000840000008400000000000000000000000000000000000000000000FFFF
      FF0084848400848484008484840084848400848484008484840084848400FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000840000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000084
      0000FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000008484840000840000008400008484
      0000FF00000084848400FFFFFF00FFFF0000FFFFFF00FFFF0000000000000000
      000000840000008400000000000000000000000000000000000000000000FFFF
      0000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000840000FFFFFF00FFFFFF0000840000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000FFFF000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF000000000000FFFF00000000000000000084848400FF000000FF000000FF00
      0000FF000000FF00000084848400FFFFFF00FFFF0000FFFFFF00000000000000
      000000840000008400000000000000000000000000000000000000000000FFFF
      FF0084848400848484008484840084848400848484008484840084848400FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF0000840000FFFFFF00FFFFFF000084000000840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000000000000000000000000000000000000000FF
      FF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      000000FFFF0000000000000000000000000084848400FF000000FF0000000084
      000000840000FF000000FF000000848484008484840000000000FF0000000084
      000000000000008400000000000000000000000000000000000000000000FFFF
      0000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF000084000000840000008400000084000000840000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      000000FFFF0000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000FF
      FF000000000000000000000000000000000000000000848484000084000000FF
      000000840000FF000000FF000000FF00000084000000FF00000084000000FF00
      000000840000000000000000000000000000000000000000000000000000FFFF
      FF00848484008484840084848400FFFFFF00FFFF0000FFFFFF00FFFF0000FFFF
      FF0000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000084000000840000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      00000000000000FFFF0000000000FFFFFF00FFFFFF000000000000FFFF000000
      0000000000000000000000000000000000000000000084848400008400000000
      000000FF00000084000000840000FF0000000084000000840000FF0000008400
      0000FF000000000000000000000000000000000000000000000000000000FFFF
      0000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF0000FFFFFF00FFFF
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF0000840000FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF00000000000000000000FFFF00000000000000
      84000000000000000000000000000000000000000000FFFFFF0084848400FFFF
      FF000000000000FF00000084000000840000008400000084000000840000FF00
      000084000000FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000000000000000000000
      8400000000000000000000000000000000000000000000000000000000008484
      840084848400FFFFFF00FFFFFF0000FF00000084000000840000008400000084
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      8400000000000000000000000000000000000000000000000000000000000000
      0000000000008484840084848400848484008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400848484000000FF000000FF000000FF000000FF000000FF008484
      84008484840000000000000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      84000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF0084848400000000000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF00848484000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF00848484000000000084848400FFFFFF00FFFFFF00FFFF
      FF000000000084000000FF00000084000000840000000000000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      000000000000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF00000000
      00000000000000000000000000000000000000000000848484000000FF000000
      FF000000FF00FFFFFF00FFFFFF000000FF000000FF000000FF00FFFFFF00FFFF
      FF000000FF000000FF000000FF008484840084848400FFFFFF00FFFFFF00FFFF
      FF0084848400FF000000FF00000000840000FF0000008400000084000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      00000000000000000000000000000000000000000000848484000000FF000000
      FF000000FF000000FF00FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF000000
      FF000000FF000000FF000000FF008484840084848400FFFFFF00FFFFFF00FFFF
      FF0084840000000000000000000000FF000084000000FF00000084000000FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF000000000000000000000000000000000000848484000000FF000000
      FF000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF000000FF000000FF000000FF008484840084848400FFFFFF00FFFFFF00FFFF
      FF008484000000000000FFFFFF00FF0000008484000000FF000084848400FFFF
      FF00FFFFFF00FFFFFF000000000000000000000000000000000000000000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF0000000000000000000000000000000000000000000000000000FFFF
      0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF0000FFFF
      0000FFFF000000000000000000000000000000000000848484000000FF000000
      FF000000FF000000FF000000FF00FFFFFF00FFFFFF00FFFFFF000000FF000000
      FF000000FF000000FF000000FF008484840084848400FFFFFF00FFFFFF00FFFF
      FF000000000084840000FFFF00008484000000FF00008484000000000000FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000FFFF0000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF0000FFFF
      00000000000000000000000000000000000000000000848484000000FF000000
      FF000000FF000000FF00FFFFFF00FFFFFF000000FF00FFFFFF00FFFFFF000000
      FF000000FF000000FF000000FF008484840084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000084840000848400008484000000000000FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      000000000000FFFF000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFF00000000
      0000000000000000000000000000000000000000000000000000848484000000
      FF000000FF00FFFFFF00FFFFFF000000FF000000FF000000FF00FFFFFF00FFFF
      FF000000FF000000FF00848484000000000084848400FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000848484000000
      FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF000000FF0084848400000000008484840084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008484
      84000000FF000000FF000000FF000000FF000000FF000000FF000000FF000000
      FF000000FF008484840000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000084848400848484000000FF000000FF000000FF000000FF000000FF008484
      8400848484000000000000000000000000008400000084000000840000008400
      0000840000008400000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084848400848484008484840084848400848484000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      0000000000000000000084000000840000008400000084000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000084848400008484008484
      8400008484008484840084000000FFFFFF008400000084000000840000008400
      00008400000084000000FFFFFF00840000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF0084000000000000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00840000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      000000000000FFFFFF00000000000000000000000000FFFFFF00000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00840000008400000084000000FFFF
      FF00840000008400000084000000840000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000000000000000
      000000000000FFFFFF0084000000000000000000000000848400848484000084
      8400848484000084840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF0084000000FFFFFF0084000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000000000000000000084848400008484008484
      8400008484008484840084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF00000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF000000000000000000FFFFFF008400
      0000840000008400000084000000000000000000000000848400848484000084
      8400848484000084840084000000840000008400000084000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000FFFFFF008400000000000000000000000000000084848400008484008484
      8400008484008484840000848400848484000084840084848400008484008484
      8400008484000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF0084000000FFFFFF00FFFFFF00FFFFFF00FFFFFF008400
      0000840000000000000000000000000000000000000000848400848484000000
      0000000000000000000000000000000000000000000000000000000000008484
      8400848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FFFFFF000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF000000000000000000000000000000000000000000FFFFFF00000000000000
      00000000000000000000000000000000000000000000FFFFFF00000000000000
      0000FFFFFF000000000084000000840000008400000084000000840000008400
      0000000000000000000000000000000000000000000084848400848484000000
      0000C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600C6C6C600000000008484
      8400008484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FFFFFF000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF0000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000848400848484000084
      84000000000000FFFF00000000000000000000FFFF0000000000848484000084
      8400848484000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000FFFF0000FFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      000000000000FFFFFF0000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FFFFFF00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000000000000084000000000000000000000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000000000000084000000000000008400000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000FFFF0000FFFF0000FFFF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000000000000000000084000000000000008400000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000848484008484840084848400000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000000000000000000000000000000000000000000000000000008400
      0000848484000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000840000008400000084000000000000008400000000000000000000008400
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000084000000000000000000000000000000000000000000
      0000840000008484840000000000000000000000000000000000848484008400
      0000000000000000000000000000000000000000000084000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000008400000084000000840000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000084000000000000008400000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000840000000000000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000000000008400
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      0000000000000000000000000000000000000000000000000000840000008400
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000008400000000000000000000000000000000000000840000000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000840000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF000000000000000000000000000000000000000000FFFFFF000000
      0000000000000000000000000000000000000000000000000000840000000000
      0000000000000000000084000000840000000000000000000000000000000000
      0000840000008484840000000000000000000000000000000000848484008400
      0000000000000000000000000000000000008400000084000000000000000000
      0000000000008400000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000008400000084000000840000008400
      0000848484000000000000000000000000000000000000000000000000008484
      8400840000008400000084000000840000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF000000000000000000000000000000000000000000FFFF
      FF00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
      FF00FFFFFF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000900000000100010000000000800400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFE1FF0000FFFFFFFFDFFF0000
      FE7FFFFFDFFF0000FC3FFBDFC1870000F81FF3CF81870000F00FE3C781870000
      E007C00381870000FC3F80019F870000FC3F80019F870000FC3FC0039E030000
      FC3FE3C79E010000FC3FF3CF80010000FC3FFBDF81030000FC3FFFFF81870000
      FFFFFFFFE1FF0000FFFFFFFFFFFF0000FFFFFC00F9F9FFFFF000FDFEE2F1FFFF
      F000FC028C61FC3FF000FC12B813FC3FF000FC02A140FC3F0000FC1280A0FC3F
      0000FC0A8040FC3F0000FC42C020FC3F00008022E010FC3F000080008020E007
      001F80018000F00F003B80038000F81F007B80078001FC3F006080078007FE7F
      007B80078007FFFF007B80078007FFFFFE7FC003F9FFFFFFF83FDFFBE6FC8000
      E11FC00B9E788000838FC00B793080000C47C00B600980007023C00B00A08000
      4011C00B005080000008C00B802080004204C00BC0108000A041C00BE0088000
      D041C00BF0108000E800C00BF8008001F401C003FE00C07FFA07C007FE00E0FF
      FD1FC00FFE01FFFFFE7FFFFFFE03FFFFFFFFE000FFFFFFFF0100EAAAFFFFFFFF
      0100E1548000C0010100E2AA8000C001C7C3E0548000C001E007E0AA8000C001
      E007E1548000C001F00FE02A8000C001F10F60548000C001F81F208A8000C001
      F81FC0008000C001FC3900008000E0FFFC3001FF8000F1FFFE7F84FF8000FFFA
      FE7026FFFFFFFFF9FFF967FFFFFFFFF8C001FFFFF839FFFFC001C003E001FFFF
      C001C003C0018003C001C003A0037FFDC001C003A1434005C001C00320A14005
      C001C00320414005C001C00300214005C001800100114005C001C00300094005
      C001E00780034005C001F00790034005C001F80788034005C003FC27E00F7FFD
      C007FE67F83F8003C00FFFFFFFFFFFFFFFFFFFFFFFFFFC1F0001FFFFFFFFF007
      0001FFFFFFFFE0030001FDFFFFBFC0010441F9FFFF9FC0010821F1FFFF8F8000
      0001E003C00780000201C003C00380000401C003C00380000821E003C0078000
      0441F1FFFF8FC0010001F9FFFF9FC0010001FDFFFFBFE0030001FFFFFFFFF007
      0001FFFFFFFFFC1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC00FFFFFFFF
      FC018000FFF907C1FC010000E7FF07C1FC010000C3F307C100010000C3E70101
      00010001E1C7000100010003F08F020100010003F81F020100030003FC3F8003
      00070003F81FC107000F0003F09FC10700FF0003C1C7E38F01FF800783E3E38F
      03FFF87F8FF1E38FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC007FFFFFFFFF3FF
      BFEBFFFFFFFFED9F0005FFFFFFFFED6F7E31FFFFFFFFED6F7E35FFF7E7FFF16F
      0006C1F3CF83FD1F7FEAC3FBDFC3FC7F8014C7FBDFE3FEFFC00ACBFBDFD3FC7F
      E001DCF3CF3BFD7FE007FF07E0FFF93FF007FFFFFFFFFBBFF003FFFFFFFFFBBF
      F803FFFFFFFFFBBFFFFFFFFFFFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object lbDockServer1: TJvDockServer
    LeftSplitterStyle.Cursor = crHSplit
    LeftSplitterStyle.ParentColor = False
    TopSplitterStyle.Cursor = crVSplit
    TopSplitterStyle.ParentColor = False
    RightSplitterStyle.Cursor = crHSplit
    RightSplitterStyle.ParentColor = False
    BottomSplitterStyle.Cursor = crVSplit
    BottomSplitterStyle.ParentColor = False
    DockStyle = JvDockVSNetStyle1
    Left = 128
    Top = 32
  end
  object PopupMenu1: TPopupMenu
    OnPopup = PopupMenu1Popup
    Left = 208
    Top = 32
    object Dockable_Item: TMenuItem
      Caption = 'Doc&kable'
      OnClick = Dockable_ItemClick
    end
    object Hide_Item: TMenuItem
      Caption = '&Hide'
      OnClick = Hide_ItemClick
    end
    object Float_Item: TMenuItem
      Caption = '&Floating'
      OnClick = Float_ItemClick
    end
    object AutoHide_Item: TMenuItem
      Caption = '&Auto Hide'
      OnClick = AutoHide_ItemClick
    end
  end
  object JvDockVSNetStyle1: TJvDockVSNetStyle
    ConjoinServerOption.GrabbersSize = 18
    ConjoinServerOption.SplitterWidth = 4
    ConjoinServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    ConjoinServerOption.ActiveFont.Color = clWhite
    ConjoinServerOption.ActiveFont.Height = -11
    ConjoinServerOption.ActiveFont.Name = 'Tahoma'
    ConjoinServerOption.ActiveFont.Style = []
    ConjoinServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    ConjoinServerOption.InactiveFont.Color = clBlack
    ConjoinServerOption.InactiveFont.Height = -11
    ConjoinServerOption.InactiveFont.Name = 'Tahoma'
    ConjoinServerOption.InactiveFont.Style = []
    ConjoinServerOption.TextAlignment = taLeftJustify
    ConjoinServerOption.ActiveTitleStartColor = 6956042
    ConjoinServerOption.ActiveTitleEndColor = 6956042
    ConjoinServerOption.InactiveTitleStartColor = clBtnFace
    ConjoinServerOption.InactiveTitleEndColor = clBtnFace
    ConjoinServerOption.TextEllipsis = True
    ConjoinServerOption.SystemInfo = True
    TabServerOption.TabPosition = tpBottom
    TabServerOption.ActiveSheetColor = clBtnFace
    TabServerOption.InactiveSheetColor = 15725559
    TabServerOption.ActiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.ActiveFont.Color = clWindowText
    TabServerOption.ActiveFont.Height = -11
    TabServerOption.ActiveFont.Name = 'MS Sans Serif'
    TabServerOption.ActiveFont.Style = []
    TabServerOption.InactiveFont.Charset = DEFAULT_CHARSET
    TabServerOption.InactiveFont.Color = 5395794
    TabServerOption.InactiveFont.Height = -11
    TabServerOption.InactiveFont.Name = 'MS Sans Serif'
    TabServerOption.InactiveFont.Style = []
    TabServerOption.HotTrackColor = clBlue
    TabServerOption.ShowTabImages = True
    ChannelOption.ActivePaneSize = 100
    ChannelOption.ShowImage = True
    ChannelOption.MouseleaveHide = False
    ChannelOption.HideHoldTime = 1000
    Left = 168
    Top = 32
  end
end
