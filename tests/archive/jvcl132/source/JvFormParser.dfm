object FormParsers: TFormParsers
  Left = 437
  Top = 279
  BorderStyle = bsDialog
  Caption = 'Parser - Edit '
  ClientHeight = 222
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 120
    Width = 317
    Height = 97
    ItemHeight = 13
    TabOrder = 4
    OnClick = ListBox1Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 10
    Width = 407
    Height = 107
    Caption = '[ Properties ]'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
    object Label1: TLabel
      Left = 14
      Top = 26
      Width = 41
      Height = 13
      Caption = 'Keyword'
    end
    object Label2: TLabel
      Left = 12
      Top = 51
      Width = 44
      Height = 13
      Caption = 'Start Tag'
    end
    object Label3: TLabel
      Left = 204
      Top = 26
      Width = 41
      Height = 13
      Caption = 'End Tag'
    end
    object Label4: TLabel
      Left = 204
      Top = 51
      Width = 41
      Height = 13
      Hint = 'Where the start text must be'
      Caption = 'Must be '
    end
    object Label5: TLabel
      Left = 12
      Top = 78
      Width = 45
      Height = 13
      Caption = 'Take text'
    end
    object Edit1: TEdit
      Left = 66
      Top = 22
      Width = 115
      Height = 21
      Hint = 
        'Put here the keyword'#13#10'you want the component to send'#13#10'when he ha' +
        's found this item'
      TabOrder = 0
      OnChange = Edit1Change
    end
    object Edit2: TEdit
      Left = 66
      Top = 47
      Width = 115
      Height = 21
      Hint = 'Put here the string that'#13#10'is just before the part'#13#10'you want'
      TabOrder = 1
      OnChange = Edit2Change
    end
    object Edit3: TEdit
      Left = 256
      Top = 22
      Width = 115
      Height = 21
      Hint = 'Put here the tag you want to find '#13#10'to end the tag'
      TabOrder = 2
      OnChange = Edit3Change
    end
    object ComboBox1: TComboBox
      Left = 66
      Top = 74
      Width = 305
      Height = 21
      Hint = 'Tell the component'#13#10'which part you want of the string'
      ItemHeight = 13
      TabOrder = 4
      Text = 'Between limits'
      OnChange = ComboBox1Change
      Items.Strings = (
        'Between limits'
        'All before start tag'
        'All after start tag'
        'The whole line if respecting the condition')
    end
    object Edit4: TEdit
      Left = 256
      Top = 46
      Width = 115
      Height = 21
      Hint = 
        'Put here the position of the start tag'#13#10#13#10'1 if you don'#39't care, '#13 +
        #10'0 if it can'#39't be in the string, '#13#10'1 if you want it in the first' +
        ' position'#13#10'2 if you want it in the second position'#13#10'....'
      TabOrder = 3
      Text = '-1'
      OnChange = Edit4Change
    end
  end
  object BUButton1: TJvButton
    Left = 336
    Top = 120
    Width = 75
    Height = 25
    Hint = 'Add an item to the list'
    Caption = '&Add'
    TabOrder = 3
    OnClick = Button1Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton2: TJvButton
    Left = 336
    Top = 144
    Width = 75
    Height = 25
    Hint = 'Delete the selected item '#13#10'from the list'
    Caption = '&Remove'
    TabOrder = 2
    OnClick = Button2Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton3: TJvButton
    Left = 336
    Top = 168
    Width = 75
    Height = 25
    Hint = 'Apply changes'
    Caption = '&Ok'
    Default = True
    TabOrder = 0
    OnClick = BUButton3Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
  object BUButton4: TJvButton
    Left = 336
    Top = 192
    Width = 75
    Height = 25
    Hint = 'Cancel Changes'
    Cancel = True
    Caption = '&Cancel'
    TabOrder = 1
    OnClick = BUButton4Click
    HotTrackFont.Charset = DEFAULT_CHARSET
    HotTrackFont.Color = clWindowText
    HotTrackFont.Height = -11
    HotTrackFont.Name = 'MS Sans Serif'
    HotTrackFont.Style = []
  end
end
