object frmValidatorsEditor: TfrmValidatorsEditor
  Left = 289
  Top = 205
  Width = 190
  Height = 269
  BorderIcons = [biSystemMenu]
  Caption = 'Validators editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 182
    Height = 22
    AutoSize = True
    Caption = 'ToolBar1'
    EdgeBorders = []
    Flat = True
    Images = ImageList1
    TabOrder = 0
    object btnNew: TToolButton
      Left = 0
      Top = 0
      Action = acNewRequired
      DropdownMenu = popNew
      Style = tbsDropDown
    end
    object btnDelete: TToolButton
      Left = 36
      Top = 0
      Action = acDelete
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 223
    Width = 182
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    SimplePanel = False
  end
  object lbValidators: TListBox
    Left = 0
    Top = 22
    Width = 182
    Height = 201
    Align = alClient
    ItemHeight = 13
    TabOrder = 2
    OnClick = lbValidatorsClick
  end
  object popNew: TPopupMenu
    Images = ImageList1
    Left = 40
    Top = 40
    object RequiredFieldValidator1: TMenuItem
      Action = acNewRequired
      Default = True
    end
    object CompareValidator1: TMenuItem
      Action = acNewCompare
    end
    object RangeValidator1: TMenuItem
      Action = acNewRange
    end
    object RegularExpressionValidator1: TMenuItem
      Action = acNewRegExp
    end
    object CustomValidator1: TMenuItem
      Action = acNewCustom
    end
  end
  object alEditor: TActionList
    Images = ImageList1
    OnUpdate = alEditorUpdate
    Left = 96
    Top = 40
    object acNewRequired: TAction
      Caption = 'Required Field Validator'
      OnExecute = acNewRequiredExecute
    end
    object acNewCompare: TAction
      Caption = 'Compare Validator'
      OnExecute = acNewCompareExecute
    end
    object acNewRange: TAction
      Caption = 'Range Validator'
      OnExecute = acNewRangeExecute
    end
    object acNewRegExp: TAction
      Caption = 'Regular Expression Validator'
      OnExecute = acNewRegExpExecute
    end
    object acNewCustom: TAction
      Caption = 'Custom Validator'
      OnExecute = acNewCustomExecute
    end
    object acDelete: TAction
      Caption = 'Delete'
      OnExecute = acDeleteExecute
    end
  end
  object ImageList1: TImageList
    Left = 120
    Top = 120
  end
end
