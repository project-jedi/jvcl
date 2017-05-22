object frmJvCharEditDlg: TfrmJvCharEditDlg
  Left = 365
  Top = 180
  Width = 535
  Height = 417
  ActiveControl = lvCharacters
  BorderWidth = 2
  Caption = 'TJvFormatEdit.Characters Editor'
  Color = clBtnFace
  Constraints.MinHeight = 175
  Constraints.MinWidth = 320
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 342
    Width = 523
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOK: TButton
      Left = 350
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      ModalResult = 1
      TabOrder = 1
    end
    object btnCancel: TButton
      Left = 438
      Top = 9
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 2
    end
    object cbFonts: TComboBox
      Left = 8
      Top = 11
      Width = 330
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnKeyPress = cbFontsKeyPress
    end
  end
  object lvCharacters: TListView
    Left = 0
    Top = 0
    Width = 523
    Height = 342
    Align = alClient
    Checkboxes = True
    Columns = <
      item
        AutoSize = True
        Caption = 'Characters'
      end>
    ColumnClick = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Shell Dlg 2'
    Font.Style = []
    HideSelection = False
    IconOptions.AutoArrange = True
    LargeImages = ImageList1
    MultiSelect = True
    ReadOnly = True
    ParentFont = False
    ParentShowHint = False
    PopupMenu = PopupMenu1
    ShowHint = True
    SmallImages = ImageList1
    TabOrder = 1
    ViewStyle = vsList
    OnAdvancedCustomDrawItem = lvCharactersAdvancedCustomDrawItem
    OnEnter = lvCharactersEnter
    OnInfoTip = lvCharactersInfoTip
    OnResize = lvCharactersResize
    OnSelectItem = lvCharactersSelectItem
  end
  object PopupMenu1: TPopupMenu
    Left = 16
    Top = 16
    object SelectAll1: TMenuItem
      Action = acCheckAll
    end
    object UnselectAll1: TMenuItem
      Action = acUnCheckAll
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object CheckSelected1: TMenuItem
      Action = acCheckSel
    end
    object UnCheckSelected1: TMenuItem
      Action = acUnCheckSel
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Special1: TMenuItem
      Caption = 'Check Special'
      object Alpha1: TMenuItem
        Action = acAlpha
      end
      object acInteger1: TMenuItem
        Action = acInteger
      end
      object acAlphaNum1: TMenuItem
        Action = acAlphaNum
      end
      object acHex1: TMenuItem
        Action = acHex
      end
      object acFloat1: TMenuItem
        Action = acFloat
      end
      object acScientific1: TMenuItem
        Action = acScientific
      end
      object acCurrency1: TMenuItem
        Action = acCurrency
      end
    end
    object Invertselection1: TMenuItem
      Action = acInvertCheck
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object View1: TMenuItem
      Caption = 'View'
      object Large1: TMenuItem
        Action = acLarge
        GroupIndex = 1
        RadioItem = True
      end
      object Small1: TMenuItem
        Action = acSmall
        GroupIndex = 1
        RadioItem = True
      end
      object List1: TMenuItem
        Action = acList
        GroupIndex = 1
        RadioItem = True
      end
      object Report1: TMenuItem
        Action = acReport
        GroupIndex = 1
        RadioItem = True
      end
    end
  end
  object ActionList1: TActionList
    Left = 48
    Top = 16
    object acCheckAll: TAction
      Category = 'General'
      Caption = 'Check All'
      OnExecute = acCheckAllExecute
    end
    object acUnCheckAll: TAction
      Category = 'General'
      Caption = 'Uncheck All'
      OnExecute = acUnCheckAllExecute
    end
    object acInvertCheck: TAction
      Category = 'General'
      Caption = 'Invert Checks'
      OnExecute = acInvertCheckExecute
    end
    object acAlpha: TAction
      Category = 'Special'
      Caption = 'Alphabetic'
      OnExecute = acAlphaExecute
    end
    object acAlphaNum: TAction
      Category = 'Special'
      Caption = 'Alpha Numeric'
      OnExecute = acAlphaNumExecute
    end
    object acHex: TAction
      Category = 'Special'
      Caption = 'Hexadecimal'
      OnExecute = acHexExecute
    end
    object acFloat: TAction
      Category = 'Special'
      Caption = 'Float'
      OnExecute = acFloatExecute
    end
    object acScientific: TAction
      Category = 'Special'
      Caption = 'Scientific'
      OnExecute = acScientificExecute
    end
    object acCurrency: TAction
      Category = 'Special'
      Caption = 'Currency'
      OnExecute = acCurrencyExecute
    end
    object acInteger: TAction
      Category = 'Special'
      Caption = 'Integer'
      OnExecute = acIntegerExecute
    end
    object acLarge: TAction
      Caption = 'Large'
      OnExecute = acLargeExecute
    end
    object acSmall: TAction
      Caption = 'Small'
      OnExecute = acSmallExecute
    end
    object acList: TAction
      Caption = 'List'
      Checked = True
      OnExecute = acListExecute
    end
    object acReport: TAction
      Caption = 'Report'
      OnExecute = acReportExecute
    end
    object acCheckSel: TAction
      Category = 'General'
      Caption = 'Check Selected'
      OnExecute = acCheckSelExecute
    end
    object acUnCheckSel: TAction
      Category = 'General'
      Caption = 'Uncheck Selected'
      OnExecute = acUnCheckSelExecute
    end
  end
  object ImageList1: TImageList
    Left = 80
    Top = 16
  end
end
