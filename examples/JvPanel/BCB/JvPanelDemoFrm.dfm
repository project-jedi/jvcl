object JvPanelDemoMainFrm: TJvPanelDemoMainFrm
  Left = 260
  Top = 125
  Width = 550
  Height = 454
  Caption = 'JvPanelDemo'
  Color = clBackground
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvPanel1: TJvPanel
    Left = 8
    Top = 8
    Width = 495
    Height = 289
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
    Sizeable = True
    MultiLine = True
    Caption = 'JvPanel Demo'
    Constraints.MinHeight = 160
    Constraints.MinWidth = 495
    TabOrder = 0
    object Label1: TLabel
      Left = 14
      Top = 232
      Width = 46
      Height = 13
      Anchors = [akLeft, akBottom]
      Caption = 'Options:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Shell Dlg 2'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Edit1: TEdit
      Left = 24
      Top = 64
      Width = 455
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object JvFilenameEdit1: TJvFilenameEdit
      Left = 24
      Top = 32
      Width = 455
      Height = 21
      AddQuotes = False
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'JvFilenameEdit1'
    end
    object CheckBox2: TCheckBox
      Left = 12
      Top = 256
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Auto Arrange'
      TabOrder = 2
      OnClick = CheckBox2Click
    end
    object CheckBox1: TCheckBox
      Left = 110
      Top = 256
      Width = 97
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = 'Transparent'
      TabOrder = 3
      OnClick = CheckBox1Click
    end
  end
  object JvFormStorage1: TJvFormStorage
    AppStoragePath = '%FORM_NAME%\'
    StoredValues = <>
    Left = 184
    Top = 56
  end
end
