object Form1: TForm1
  Left = 236
  Top = 227
  Width = 620
  Height = 412
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvOutlookPanel1: TJvOutlookPanel
    Left = 0
    Top = 0
    Width = 161
    Height = 366
    Align = alLeft
    Color = clGrayText
    Title.Caption = 'Outlook Panel'
    Title.Font.Charset = DEFAULT_CHARSET
    Title.Font.Color = clWindowText
    Title.Font.Height = -11
    Title.Font.Name = 'MS Sans Serif'
    Title.Font.Style = [fsBold]
    object Edit1: TEdit
      Left = 20
      Top = 26
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'Edit1'
    end
  end
  object JvOutlookPanel3: TJvOutlookPanel
    Left = 161
    Top = 0
    Width = 451
    Height = 366
    Align = alClient
    Color = clMaroon
    Title.Caption = 'Outlook Panel'
    Title.Font.Charset = DEFAULT_CHARSET
    Title.Font.Color = clWindowText
    Title.Font.Height = -11
    Title.Font.Name = 'MS Sans Serif'
    Title.Font.Style = [fsBold]
    object CheckBox1: TCheckBox
      Left = 20
      Top = 28
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 1
    end
    object JvOutlookPanel2: TJvOutlookPanel
      Left = 98
      Top = 114
      Width = 243
      Height = 133
      Color = clBlack
      Title.Caption = 'Outlook Panel'
      Title.Font.Charset = DEFAULT_CHARSET
      Title.Font.Color = clWindowText
      Title.Font.Height = -11
      Title.Font.Name = 'MS Sans Serif'
      Title.Font.Style = [fsBold]
      object Button1: TButton
        Left = 74
        Top = 28
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 1
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 374
    Top = 6
    object File1: TMenuItem
      Caption = 'File'
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object Panels1: TMenuItem
      Caption = 'Panels'
      OnClick = Panels1Click
      object Panel11: TMenuItem
        Caption = 'Panel1'
        OnClick = Panel11Click
      end
      object Panel21: TMenuItem
        Caption = 'Panel2'
        OnClick = Panel21Click
      end
      object Panel31: TMenuItem
        Caption = 'Panel3'
        OnClick = Panel31Click
      end
    end
  end
end
