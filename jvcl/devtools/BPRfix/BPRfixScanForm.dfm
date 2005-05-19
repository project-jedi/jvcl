object FormScan: TFormScan
  Left = 326
  Top = 240
  BorderStyle = bsDialog
  Caption = 'Scan for files...'
  ClientHeight = 69
  ClientWidth = 582
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDirectory: TLabel
    Left = 40
    Top = 8
    Width = 69
    Height = 13
    Caption = 'LabelDirectory'
  end
  object Animate1: TAnimate
    Left = 8
    Top = 8
    Width = 16
    Height = 16
    CommonAVI = aviFindFile
    StopFrame = 8
  end
  object ButtonCancel: TButton
    Left = 200
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
end
