object MemoEditFrm: TMemoEditFrm
  Left = 427
  Top = 205
  Width = 305
  Height = 305
  ActiveControl = reLines
  BorderStyle = bsSizeToolWin
  Caption = 'Memo for %s'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 234
    Width = 297
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnOK: TButton
      Left = 129
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = '&OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 209
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akRight, akBottom]
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object reLines: TRichEdit
    Left = 0
    Top = 0
    Width = 297
    Height = 234
    Align = alClient
    PopupMenu = popMemo
    ScrollBars = ssBoth
    TabOrder = 1
    OnKeyUp = reLinesKeyUp
  end
  object popMemo: TPopupMenu
    Left = 128
    Top = 104
    object Load1: TMenuItem
      Caption = 'Load...'
      ShortCut = 16463
      OnClick = Load1Click
    end
    object Save1: TMenuItem
      Caption = 'Save...'
      ShortCut = 16467
      OnClick = Save1Click
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Cut1: TMenuItem
      Caption = 'Cut'
      ShortCut = 16472
      OnClick = Cut1Click
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      ShortCut = 16451
      OnClick = Copy1Click
    end
    object Paste1: TMenuItem
      Caption = 'Paste'
      ShortCut = 16470
      OnClick = Paste1Click
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Selectall1: TMenuItem
      Caption = 'Select all'
      ShortCut = 16449
      OnClick = Selectall1Click
    end
  end
end
