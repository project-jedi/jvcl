object frmUnitStats: TfrmUnitStats
  Left = 399
  Top = 172
  BorderStyle = bsDialog
  Caption = 'Statistics for "%s"'
  ClientHeight = 344
  ClientWidth = 376
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    376
    344)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 16
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 8
    Top = 72
    Width = 43
    Height = 13
    Caption = 'Used by:'
  end
  object Label3: TLabel
    Left = 8
    Top = 184
    Width = 27
    Height = 13
    Caption = 'Uses:'
  end
  object edName: TEdit
    Left = 8
    Top = 32
    Width = 358
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 0
  end
  object reUsed: TRichEdit
    Left = 8
    Top = 88
    Width = 358
    Height = 89
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    PlainText = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object reUses: TRichEdit
    Left = 8
    Top = 200
    Width = 358
    Height = 85
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBtnFace
    PlainText = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object btnOK: TButton
    Left = 150
    Top = 306
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
end
