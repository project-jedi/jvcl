object JvHTMLParserForm: TJvHTMLParserForm
  Tag = 1
  Left = 437
  Top = 279
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Parser - Edit '
  ClientHeight = 242
  ClientWidth = 423
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100001001000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    0000000080000080000000808000800000008000800080800000C0C0C0008080
    80000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    00000000000000000BBBB0000000000BB000BB000000000BB0000B000000000B
    BB000BB00000000BBB000BB00000000000000BB00000000000000BB000000000
    00000BB00000000000000BB00000000000000BB00000000000000BB000000000
    00000BB0000000000000BBBB00000000000BBBBBB0000000000000000000FFFF
    0000F87F0000E73F0000E7BF0000E39F0000E39F0000FF9F0000FF9F0000FF9F
    0000FF9F0000FF9F0000FF9F0000FF9F0000FF0F0000FE070000FFFF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 120
    Width = 321
    Height = 117
    ItemHeight = 13
    TabOrder = 4
    OnClick = ListBox1Click
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 10
    Width = 407
    Height = 107
    Caption = ' Properties '
    Enabled = False
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
    object edKeyword: TEdit
      Left = 66
      Top = 22
      Width = 115
      Height = 21
      Hint = 
        'Put here the keyword'#13#10'you want the component to send'#13#10'when he ha' +
        's found this item'
      TabOrder = 0
      OnChange = edKeywordChange
    end
    object edStartTag: TEdit
      Left = 66
      Top = 47
      Width = 115
      Height = 21
      Hint = 'Put here the string that'#13#10'is just before the part'#13#10'you want'
      TabOrder = 1
      OnChange = edStartTagChange
    end
    object edEndTag: TEdit
      Left = 256
      Top = 22
      Width = 115
      Height = 21
      Hint = 'Put here the tag you want to find '#13#10'to end the tag'
      TabOrder = 2
      OnChange = edEndTagChange
    end
    object cbTakeText: TComboBox
      Left = 66
      Top = 74
      Width = 305
      Height = 21
      Hint = 'Tell the component'#13#10'which part you want of the string'
      ItemHeight = 13
      TabOrder = 4
      Text = 'Between limits'
      OnChange = cbTakeTextChange
      Items.Strings = (
        'Between limits'
        'All before start tag'
        'All after start tag'
        'The whole line if respecting the condition')
    end
    object edMustBe: TEdit
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
      OnChange = edMustBeChange
    end
  end
  object btnAdd: TButton
    Left = 339
    Top = 120
    Width = 75
    Height = 25
    Hint = 'Add an item to the list'
    Caption = '&Add'
    TabOrder = 3
    OnClick = Button1Click
  end
  object btnRemove: TButton
    Left = 339
    Top = 152
    Width = 75
    Height = 25
    Hint = 'Delete the selected item '#13#10'from the list'
    Caption = '&Remove'
    TabOrder = 2
    OnClick = Button2Click
  end
  object OkBtn: TButton
    Left = 339
    Top = 184
    Width = 75
    Height = 25
    Hint = 'Apply changes'
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 339
    Top = 216
    Width = 75
    Height = 25
    Hint = 'Cancel Changes'
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
    OnClick = CancelBtnClick
  end
end
