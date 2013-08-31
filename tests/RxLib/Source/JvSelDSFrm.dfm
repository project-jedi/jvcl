object JvSelectDataSetForm: TJvSelectDataSetForm
  Left = 336
  Top = 201
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Memory DataSet Structure'
  ClientHeight = 191
  ClientWidth = 254
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox: TGroupBox
    Left = 9
    Top = 3
    Width = 235
    Height = 143
    Caption = ' Borrow Structure From '
    TabOrder = 0
    object DataSetList: TListBox
      Left = 8
      Top = 19
      Width = 218
      Height = 115
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
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
