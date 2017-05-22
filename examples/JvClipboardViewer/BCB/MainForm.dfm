object frmMain: TfrmMain
  Left = 192
  Top = 107
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JvClipboardViewer Demo'
  ClientHeight = 453
  ClientWidth = 705
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
  object Label3: TLabel
    Left = 379
    Top = 12
    Width = 83
    Height = 13
    Caption = 'Clipboard content'
  end
  object Label2: TLabel
    Left = 11
    Top = 128
    Width = 287
    Height = 13
    Caption = 'Clipboard as image (Try the print screen/Alt+print screen key)'
  end
  object Label1: TLabel
    Left = 11
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
    Anchors = [akLeft, akTop, akBottom]
    Stretch = True
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
    Left = 378
    Top = 32
    Width = 319
    Height = 409
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    OnImage = JvClipboardViewer1Image
    OnText = JvClipboardViewer1Text
  end
end
