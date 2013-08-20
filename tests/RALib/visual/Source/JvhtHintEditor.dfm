object HintEditor: TJvHintEditor
  Left = 200
  Top = 108
  BorderStyle = bsDialog
  Caption = 'RAhtHint Editor'
  ClientHeight = 268
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = True
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 19
    Height = 13
    Caption = 'Hint'
  end
  object Label2: TLabel
    Left = 232
    Top = 8
    Width = 177
    Height = 13
    Alignment = taRightJustify
    Caption = 'Drag mouse over this label to see hint'
    WordWrap = True
  end
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 401
    Height = 185
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
    OnChange = Memo1Change
    OnKeyDown = Memo1KeyDown
  end
  object bOk: TButton
    Left = 248
    Top = 232
    Width = 75
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object bCancel: TButton
    Left = 334
    Top = 232
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
