object FormListb: TFormListb
  Left = 379
  Top = 317
  BorderStyle = bsDialog
  Caption = 'JEDI - Listboxes'
  ClientHeight = 352
  ClientWidth = 483
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
  object Bevel1: TBevel
    Left = 0
    Top = 305
    Width = 483
    Height = 47
    Align = alBottom
    Shape = bsTopLine
  end
  object lblColumn1: TJvLabel
    Left = 8
    Top = 4
    Width = 56
    Height = 13
    Caption = 'First column'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object lblColumn2: TJvLabel
    Left = 258
    Top = 4
    Width = 75
    Height = 13
    Caption = 'Second Column'
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object ListBox1: TListBox
    Left = 7
    Top = 20
    Width = 208
    Height = 278
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 0
    OnDblClick = btnRightClick
  end
  object ListBox2: TListBox
    Left = 258
    Top = 20
    Width = 213
    Height = 277
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    MultiSelect = True
    TabOrder = 1
    OnDblClick = btnLeftClick
  end
  object btnLeft: TButton
    Left = 225
    Top = 96
    Width = 25
    Height = 25
    Caption = '<'
    TabOrder = 3
    OnClick = btnLeftClick
  end
  object btnRight: TButton
    Left = 225
    Top = 64
    Width = 25
    Height = 25
    Caption = '>'
    TabOrder = 2
    OnClick = btnRightClick
  end
  object btnOK: TButton
    Left = 309
    Top = 315
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
  end
  object btnCancel: TButton
    Left = 389
    Top = 315
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
end
