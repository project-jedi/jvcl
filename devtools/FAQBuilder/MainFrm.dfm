object frmMain: TfrmMain
  Left = 339
  Top = 167
  Width = 700
  Height = 445
  Caption = 'FAQ Builder'
  Color = clBtnFace
  Constraints.MinHeight = 390
  Constraints.MinWidth = 550
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  Menu = mmMain
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object vertSplitter: TJvNetscapeSplitter
    Left = 192
    Top = 2
    Height = 378
    Cursor = crSizeWE
    Align = alLeft
    AutoSnap = False
    MinSize = 1
    Maximized = False
    Minimized = False
    ButtonCursor = crDefault
  end
  object Bevel2: TBevel
    Left = 0
    Top = 0
    Width = 692
    Height = 2
    Align = alTop
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 2
    Width = 192
    Height = 378
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    object Label3: TLabel
      Left = 4
      Top = 4
      Width = 184
      Height = 15
      Align = alTop
      AutoSize = False
      Caption = ' &Items:'
      FocusControl = lvItems
      Layout = tlCenter
    end
    object lvItems: TJvDotNetListView
      Left = 4
      Top = 19
      Width = 184
      Height = 355
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = 'Items'
        end>
      ColumnClick = False
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      PopupMenu = popItems
      ShowColumnHeaders = False
      TabOrder = 0
      ViewStyle = vsReport
      OnDeletion = lvItemsDeletion
      OnSelectItem = lvItemsSelectItem
      ColumnsOrder = '0=180'
    end
  end
  object pnlRight: TPanel
    Left = 202
    Top = 2
    Width = 490
    Height = 378
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object Label2: TLabel
      Left = 3
      Top = 178
      Width = 484
      Height = 18
      Align = alTop
      AutoSize = False
      Caption = ' &Answer:'
      FocusControl = reAnswer
      Layout = tlCenter
    end
    object Label1: TLabel
      Left = 3
      Top = 3
      Width = 484
      Height = 16
      Align = alTop
      AutoSize = False
      Caption = ' &Question:'
      FocusControl = reQuestion
      Layout = tlCenter
    end
    object horzSplitter: TJvNetscapeSplitter
      Left = 3
      Top = 168
      Width = 484
      Height = 10
      Cursor = crSizeNS
      Align = alTop
      AutoSnap = False
      MinSize = 35
      Maximized = False
      Minimized = False
      ButtonCursor = crDefault
    end
    object reAnswer: TJvDotNetRichEdit
      Left = 3
      Top = 196
      Width = 484
      Height = 179
      Align = alClient
      PlainText = True
      TabOrder = 1
      WordWrap = False
      Zoom = 100
      OnEnter = reAnswerEnter
    end
    object reQuestion: TJvDotNetRichEdit
      Left = 3
      Top = 19
      Width = 484
      Height = 149
      Align = alTop
      PlainText = True
      TabOrder = 0
      WordWrap = False
      Zoom = 100
      OnEnter = reQuestionEnter
    end
  end
  object sbStatus: TStatusBar
    Left = 0
    Top = 380
    Width = 692
    Height = 19
    Panels = <
      item
        Width = 350
      end
      item
        Width = 100
      end
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object alMain: TActionList
    OnUpdate = alMainUpdate
    Left = 56
    Top = 92
    object acAdd: TAction
      Caption = '&Add'
      ShortCut = 16449
      OnExecute = acAddExecute
    end
    object acDelete: TAction
      Caption = '&Delete'
      ShortCut = 16452
      OnExecute = acDeleteExecute
    end
    object acExportHTML: TAction
      Caption = 'Export to &HTML...'
      ShortCut = 16453
      OnExecute = acExportHTMLExecute
    end
    object acSave: TAction
      Caption = '&Save'
      ShortCut = 16467
      OnExecute = acSaveExecute
    end
    object acLoad: TAction
      Caption = '&Open...'
      ShortCut = 16463
      OnExecute = acLoadExecute
    end
    object acReplace: TAction
      Caption = '&Replace'
      ShortCut = 16466
      OnExecute = acReplaceExecute
    end
    object acSaveAs: TAction
      Caption = 'Save As...'
      OnExecute = acSaveAsExecute
    end
    object acAbout: TAction
      Caption = 'About...'
      OnExecute = acAboutExecute
    end
    object acOptions: TAction
      Caption = 'Options...'
      ShortCut = 32781
      OnExecute = acOptionsExecute
    end
    object acNew: TAction
      Caption = 'New'
      ShortCut = 16462
      OnExecute = acNewExecute
    end
    object acMoveUp: TAction
      Caption = 'Move Up'
      ShortCut = 24614
      OnExecute = acMoveUpExecute
    end
    object acMoveDown: TAction
      Caption = 'Move Down'
      ShortCut = 24616
      OnExecute = acMoveDownExecute
    end
    object acHelp: TAction
      Caption = 'Help'
      ShortCut = 112
      OnExecute = acHelpExecute
    end
    object acNext: TAction
      Caption = 'Next'
      ShortCut = 117
      OnExecute = acNextExecute
    end
    object acPrev: TAction
      Caption = 'Previous'
      ShortCut = 8309
      OnExecute = acPrevExecute
    end
    object acNextItem: TAction
      Caption = 'Next'
      ShortCut = 16424
      OnExecute = acNextItemExecute
    end
    object acPrevItem: TAction
      Caption = 'Previous'
      ShortCut = 16422
      OnExecute = acPrevItemExecute
    end
    object acFullScreen: TAction
      Caption = 'Full Screen'
      ShortCut = 122
      OnExecute = acFullScreenExecute
    end
  end
  object mmMain: TMainMenu
    Left = 90
    Top = 90
    object File1: TMenuItem
      Caption = 'File'
      object New1: TMenuItem
        Action = acNew
      end
      object Open1: TMenuItem
        Action = acLoad
      end
      object Save1: TMenuItem
        Action = acSave
      end
      object SaveAs1: TMenuItem
        Action = acSaveAs
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object SaveAsHTML1: TMenuItem
        Action = acExportHTML
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        ShortCut = 32883
        OnClick = Exit1Click
      end
    end
    object Edit1: TMenuItem
      Caption = 'Edit'
      object Add1: TMenuItem
        Action = acAdd
      end
      object Replace1: TMenuItem
        Action = acReplace
      end
      object Delete1: TMenuItem
        Action = acDelete
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Options1: TMenuItem
        Action = acOptions
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object FullScreen1: TMenuItem
        Action = acFullScreen
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object Help2: TMenuItem
        Action = acHelp
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object About1: TMenuItem
        Action = acAbout
      end
    end
  end
  object ppHTMLBuilder: TPageProducer
    OnHTMLTag = ppHTMLBuilderHTMLTag
    Left = 144
    Top = 90
  end
  object popItems: TPopupMenu
    Left = 24
    Top = 72
    object Previous1: TMenuItem
      Action = acPrevItem
    end
    object Next1: TMenuItem
      Action = acNextItem
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object MoveUp1: TMenuItem
      Action = acMoveUp
    end
    object MoveDown1: TMenuItem
      Action = acMoveDown
    end
  end
end
