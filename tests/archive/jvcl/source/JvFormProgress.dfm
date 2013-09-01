object FormProg: TFormProg
  Left = 574
  Top = 453
  Anchors = [akLeft, akTop, akRight, akBottom]
  BorderStyle = bsNone
  Caption = 'FormProg'
  ClientHeight = 60
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 314
    Height = 60
    Align = alClient
    BevelInner = bvRaised
    BorderStyle = bsSingle
    Ctl3D = True
    ParentCtl3D = False
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 10
      Width = 41
      Height = 13
      Caption = 'Progress'
    end
    object ProgressBar1: TProgressBar
      Left = 16
      Top = 28
      Width = 279
      Height = 15
      Min = 0
      Max = 100
      TabOrder = 0
    end
  end
end
