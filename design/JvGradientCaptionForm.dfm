object GradCaptionsEditor: TGradCaptionsEditor
  Left = 391
  Top = 254
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Caption Editor'
  ClientHeight = 210
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001001000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000BBBB0000000000BB000BB000000000BB0000B000000000B
    BB000BB00000000BBB000BB00000000000000BB00000000000000BB000000000
    00000BB00000000000000BB00000000000000BB00000000000000BB000000000
    00000BB0000000000000BBBB00000000000BBBBBB0000000000000000000FFFF
    0000F87F0000E73F0000E7BF0000E39F0000E39F0000FF9F0000FF9F0000FF9F
    0000FF9F0000FF9F0000FF9F0000FF9F0000FF0F0000FE070000FFFF0000}
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object ApplyButton: TButton
    Left = 312
    Top = 176
    Width = 75
    Height = 25
    Caption = '&Apply'
    TabOrder = 4
    OnClick = ApplyButtonClick
  end
  object CancelButton: TButton
    Left = 232
    Top = 176
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object OkButton: TButton
    Left = 152
    Top = 176
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = OkButtonClick
  end
  object GroupBox2: TGroupBox
    Left = 152
    Top = 4
    Width = 233
    Height = 157
    Caption = 'Caption properties'
    TabOrder = 1
    object Label1: TLabel
      Left = 12
      Top = 21
      Width = 66
      Height = 13
      Caption = 'Caption &Text: '
      FocusControl = CaptionText
    end
    object Label3: TLabel
      Left = 12
      Top = 46
      Width = 71
      Height = 13
      Caption = '&Inactive Color: '
      FocusControl = CaptionInactiveColor
    end
    object Label2: TLabel
      Left = 12
      Top = 70
      Width = 69
      Height = 13
      Caption = 'Caption &Font:  '
      FocusControl = CaptionInactiveColor
    end
    object CaptionText: TEdit
      Left = 88
      Top = 18
      Width = 133
      Height = 21
      TabOrder = 0
      OnChange = CaptionTextChange
      OnExit = ControlExit
    end
    object CaptionInactiveColor: TComboBox
      Left = 88
      Top = 43
      Width = 133
      Height = 21
      ItemHeight = 13
      TabOrder = 1
      OnDblClick = CaptionInactiveColorDblClick
      OnExit = ControlExit
    end
    object CaptionParentFont: TCheckBox
      Left = 88
      Top = 95
      Width = 137
      Height = 17
      Caption = '&Parent Font'
      TabOrder = 3
      OnClick = CheckBoxClick
      OnExit = ControlExit
    end
    object CaptionGlueNext: TCheckBox
      Left = 88
      Top = 115
      Width = 137
      Height = 17
      Caption = '&Glue Next Caption'
      TabOrder = 5
      OnClick = CheckBoxClick
      OnExit = ControlExit
    end
    object CaptionVisible: TCheckBox
      Left = 88
      Top = 135
      Width = 137
      Height = 17
      Caption = '&Visible'
      TabOrder = 4
      OnClick = CheckBoxClick
      OnExit = ControlExit
    end
    object CaptionFont: TJvComboEdit
      Left = 88
      Top = 66
      Width = 133
      Height = 21
      ButtonFlat = False
      ButtonWidth = 17
      ImageKind = ikEllipsis
      TabOrder = 2
      OnButtonClick = CaptionFontButtonClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 4
    Width = 137
    Height = 157
    Caption = '&Captions'
    TabOrder = 0
    object NewButton: TButton
      Left = 8
      Top = 124
      Width = 57
      Height = 25
      Caption = '&New'
      TabOrder = 1
      OnClick = NewButtonClick
    end
    object DeleteButton: TButton
      Left = 72
      Top = 124
      Width = 57
      Height = 25
      Caption = '&Delete'
      TabOrder = 0
      OnClick = DeleteButtonClick
    end
    object CaptionList: TListBox
      Left = 8
      Top = 16
      Width = 121
      Height = 105
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 24
    Top = 76
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 84
    Top = 76
  end
  object GradientCaption: TJvGradientCaption
    Captions = <>
    FormCaption = 'Caption Editor'
    Left = 24
    Top = 24
  end
  object FormStorage: TJvFormStorage
    AppStorage = AppStorage
    StoredValues = <
      item
      end>
    Left = 80
    Top = 24
  end
  object AppStorage: TJvAppRegistryStorage
    StorageOptions.BooleanStringTrueValues = 'TRUE, YES, Y'
    StorageOptions.BooleanStringFalseValues = 'FALSE, NO, N'
    SubStorages = <>
    Left = 133
    Top = 24
  end
end
