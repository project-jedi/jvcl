object GradCaptionsEditor: TGradCaptionsEditor
  Left = 391
  Top = 254
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
      GlyphKind = gkEllipsis
      ButtonWidth = 17
      NumGlyphs = 1
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
    object CaptionList: TJvTextListBox
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
  object GradientCaption: TJvxGradientCaption
    Captions = <>
    FormCaption = 'Caption Editor'
    Left = 24
    Top = 24
  end
  object FormStorage: TJvFormStorage
    Active = False
    UseRegistry = True
    StoredValues = <
      item
      end>
    Left = 80
    Top = 24
  end
end
