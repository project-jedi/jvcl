object frmProgress: TfrmProgress
  Left = 411
  Top = 226
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Caption'
  ClientHeight = 165
  ClientWidth = 316
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object imProgress: TImage
    Left = 16
    Top = 12
    Width = 48
    Height = 48
    AutoSize = True
    Enabled = False
    IncrementalDisplay = True
    Visible = False
  end
  object Label1: TLabel
    Left = 16
    Top = 70
    Width = 26
    Height = 13
    Caption = 'Label'
  end
  object pbProgress: TProgressBar
    Left = 16
    Top = 88
    Width = 284
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Min = 0
    Max = 100
    TabOrder = 0
  end
  object btnCancel: TButton
    Left = 216
    Top = 124
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object tmProgress: TTimer
    Enabled = False
    OnTimer = tmProgressTimer
    Left = 102
    Top = 18
  end
end
