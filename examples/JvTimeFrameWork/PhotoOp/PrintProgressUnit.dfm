object PrintProgress: TPrintProgress
  Left = 613
  Top = 250
  BorderIcons = []
  BorderStyle = bsSingle
  Caption = 'Print Progress'
  ClientHeight = 161
  ClientWidth = 367
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Label1: TLabel
    Left = 16
    Top = 8
    Width = 337
    Height = 33
    AutoSize = False
    Caption = 'Generating Report.  This may take a while.  Please be patient.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 56
    Width = 337
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = 'Processing...'
  end
  object ProgressBar1: TProgressBar
    Left = 16
    Top = 80
    Width = 337
    Height = 20
    Min = 0
    Max = 200
    TabOrder = 0
  end
  object CancelButton: TBitBtn
    Left = 140
    Top = 120
    Width = 87
    Height = 25
    TabOrder = 1
    OnClick = CancelButtonClick
    Kind = bkCancel
  end
end
