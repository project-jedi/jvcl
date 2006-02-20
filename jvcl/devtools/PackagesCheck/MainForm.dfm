object PackageCheckForm: TPackageCheckForm
  Left = 427
  Top = 146
  Caption = 'Package Check'
  ClientHeight = 276
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    337
    276)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelModel: TLabel
    Left = 8
    Top = 8
    Width = 32
    Height = 13
    Caption = '&Model:'
    FocusControl = ComboBoxModel
  end
  object ButtonTargetDefines: TButton
    Left = 8
    Top = 31
    Width = 153
    Height = 25
    Caption = 'Edit target defines'
    TabOrder = 0
    OnClick = ButtonTargetDefinesClick
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 254
    Width = 337
    Height = 22
    Panels = <>
  end
  object ComboBoxModel: TComboBox
    Left = 134
    Top = 4
    Width = 195
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object MemoMessages: TMemo
    Left = 8
    Top = 87
    Width = 321
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 3
    WordWrap = False
  end
  object ButtonGo: TButton
    Left = 168
    Top = 31
    Width = 161
    Height = 25
    Caption = 'Go'
    Default = True
    TabOrder = 4
    OnClick = ButtonGoClick
  end
  object CheckBoxCLX: TCheckBox
    Left = 8
    Top = 64
    Width = 153
    Height = 17
    Caption = 'Parse CLX targets'
    TabOrder = 5
  end
  object CheckBoxHaltOnError: TCheckBox
    Left = 168
    Top = 64
    Width = 161
    Height = 17
    Caption = 'Halt on message'
    TabOrder = 6
  end
  object JvDualListDialogSelect: TJvDualListDialog
    Sorted = False
    Label1Caption = '&Enabled'
    Label2Caption = '&Disabled'
    HelpContext = 0
    Left = 104
    Top = 80
  end
end
