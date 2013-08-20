object GradCaptionsEditor: TGradCaptionsEditor
  Left = 215
  Top = 138
  BorderStyle = bsDialog
  Caption = 'Caption Editor'
  ClientHeight = 210
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
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
      TabOrder = 2
      OnClick = CheckBoxClick
      OnExit = ControlExit
    end
    object CaptionGlueNext: TCheckBox
      Left = 88
      Top = 115
      Width = 137
      Height = 17
      Caption = '&Glue Next Caption'
      TabOrder = 4
      OnClick = CheckBoxClick
      OnExit = ControlExit
    end
    object CaptionVisible: TCheckBox
      Left = 88
      Top = 135
      Width = 137
      Height = 17
      Caption = '&Visible'
      TabOrder = 3
      OnClick = CheckBoxClick
      OnExit = ControlExit
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
  end
  object FontDialog: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    MinFontSize = 0
    MaxFontSize = 0
    Left = 72
    Top = 172
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 100
    Top = 172
  end
end
