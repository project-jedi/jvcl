object JvSelectDataSetForm: TJvSelectDataSetForm
  Left = 336
  Top = 201
  Width = 262
  Height = 221
  BorderIcons = [biSystemMenu]
  Caption = 'Memory DataSet Structure'
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
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox: TGroupBox
    Left = 9
    Top = 3
    Width = 235
    Height = 143
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Borrow Structure From '
    TabOrder = 0
    object DataSetList: TListBox
      Left = 8
      Top = 19
      Width = 218
      Height = 115
      Anchors = [akLeft, akTop, akRight, akBottom]
      Enabled = False
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnDblClick = DataSetListDblClick
      OnKeyPress = DataSetListKeyPress
    end
  end
  object OkBtn: TButton
    Left = 87
    Top = 155
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 170
    Top = 155
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
