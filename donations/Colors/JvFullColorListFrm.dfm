object JvFullColorListForm: TJvFullColorListForm
  Left = 388
  Top = 284
  BorderStyle = bsDialog
  Caption = 'Color list editor'
  ClientHeight = 398
  ClientWidth = 422
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ListBoxColors: TListBox
    Left = 8
    Top = 8
    Width = 313
    Height = 377
    Style = lbOwnerDrawFixed
    ItemHeight = 20
    MultiSelect = True
    TabOrder = 0
    OnDrawItem = ListBoxColorsDrawItem
  end
  object ButtonNew: TButton
    Left = 336
    Top = 8
    Width = 75
    Height = 25
    Action = ActionNew
    TabOrder = 1
  end
  object ButtonModify: TButton
    Left = 336
    Top = 88
    Width = 75
    Height = 25
    Action = ActionModify
    TabOrder = 2
  end
  object ButtonDelete: TButton
    Left = 336
    Top = 128
    Width = 75
    Height = 25
    Action = ActionDelete
    TabOrder = 3
  end
  object Button4: TButton
    Left = 336
    Top = 320
    Width = 75
    Height = 25
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object ButtonOK: TButton
    Left = 336
    Top = 280
    Width = 75
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 5
  end
  object BitBtnMoveUp: TBitBtn
    Left = 336
    Top = 208
    Width = 25
    Height = 25
    Action = ActionMoveUp
    TabOrder = 6
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333000333
      3333333333777F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333FF7F7FFFF333333000090000
      3333333777737777F333333099999990333333373F3333373333333309999903
      333333337F33337F33333333099999033333333373F333733333333330999033
      3333333337F337F3333333333099903333333333373F37333333333333090333
      33333333337F7F33333333333309033333333333337373333333333333303333
      333333333337F333333333333330333333333333333733333333}
    NumGlyphs = 2
  end
  object BitBtnMoveDown: TBitBtn
    Left = 336
    Top = 240
    Width = 25
    Height = 25
    Action = ActionMoveDown
    TabOrder = 7
    Glyph.Data = {
      76010000424D7601000000000000760000002800000020000000100000000100
      04000000000000010000120B0000120B00001000000000000000000000000000
      800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
      333333333337F33333333333333033333333333333373F333333333333090333
      33333333337F7F33333333333309033333333333337373F33333333330999033
      3333333337F337F33333333330999033333333333733373F3333333309999903
      333333337F33337F33333333099999033333333373333373F333333099999990
      33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333309033333333333337F7F333333333333090333
      33333333337F7F33333333333300033333333333337773333333}
    NumGlyphs = 2
  end
  object ButtonApply: TButton
    Left = 336
    Top = 360
    Width = 75
    Height = 25
    Caption = '&Apply'
    TabOrder = 8
    OnClick = ButtonApplyClick
  end
  object Button1: TButton
    Left = 336
    Top = 168
    Width = 75
    Height = 25
    Action = ActionClear
    TabOrder = 9
  end
  object Button2: TButton
    Left = 336
    Top = 48
    Width = 75
    Height = 25
    Action = ActionInsert
    TabOrder = 10
  end
  object JvFullColorDialog: TJvFullColorDialog
    FullColor = 83886079
    HelpContext = 0
    OnApply = JvFullColorDialogApply
    Left = 88
    Top = 88
  end
  object ActionList: TActionList
    Left = 88
    Top = 128
    object ActionNew: TAction
      Caption = '&New ...'
      ShortCut = 16462
      OnExecute = ActionNewExecute
      OnUpdate = ActionNewUpdate
    end
    object ActionInsert: TAction
      Caption = '&Insert'
      ShortCut = 16457
      OnExecute = ActionInsertExecute
      OnUpdate = ActionInsertUpdate
    end
    object ActionModify: TAction
      Caption = '&Modify ...'
      ShortCut = 16461
      OnExecute = ActionModifyExecute
      OnUpdate = ActionModifyUpdate
    end
    object ActionDelete: TAction
      Caption = 'D&elete'
      ShortCut = 16453
      OnExecute = ActionDeleteExecute
      OnUpdate = ActionDeleteUpdate
    end
    object ActionClear: TAction
      Caption = '&Clear'
      ShortCut = 16451
      OnExecute = ActionClearExecute
      OnUpdate = ActionClearUpdate
    end
    object ActionMoveDown: TAction
      ShortCut = 16452
      OnExecute = ActionMoveDownExecute
      OnUpdate = ActionMoveDownUpdate
    end
    object ActionMoveUp: TAction
      ShortCut = 16469
      OnExecute = ActionMoveUpExecute
      OnUpdate = ActionMoveUpUpdate
    end
  end
end
