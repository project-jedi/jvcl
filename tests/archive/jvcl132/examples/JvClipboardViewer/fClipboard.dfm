object Form1: TForm1
  Left = 283
  Top = 169
  BorderStyle = bsDialog
  Caption = 'Form1'
  ClientHeight = 453
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 147
    Top = 12
    Width = 78
    Height = 13
    Caption = 'Clipboard as text'
  end
  object Image1: TImage
    Left = 10
    Top = 148
    Width = 355
    Height = 295
    Stretch = True
  end
  object Label2: TLabel
    Left = 43
    Top = 128
    Width = 287
    Height = 13
    Caption = 'Clipboard as image (Try the print screen/Alt+print screen key)'
  end
  object Memo1: TMemo
    Left = 10
    Top = 32
    Width = 355
    Height = 89
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object JvClipboardViewer1: TJvClipboardViewer
    OnImage = JvClipboardViewer1Image
    OnText = JvClipboardViewer1Text
    Left = 66
    Top = 40
  end
end
