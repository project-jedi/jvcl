object Form1: TForm1
  Left = 110
  Top = 120
  Width = 442
  Height = 381
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 72
    Width = 72
    Height = 13
    Caption = 'Enter text here:'
  end
  object Button1: TButton
    Left = 8
    Top = 8
    Width = 113
    Height = 25
    Caption = 'ShowMessage'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 88
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'Edit1'
    OnChange = Edit1Change
  end
  object Memo1: TMemo
    Left = 136
    Top = 16
    Width = 137
    Height = 97
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
    OnChange = Memo1Change
  end
  object ComboBox1: TComboBox
    Left = 8
    Top = 120
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnDropDown = ComboBox1DropDown
  end
  object GroupBox1: TGroupBox
    Left = 280
    Top = 8
    Width = 145
    Height = 105
    Caption = 'Drag'#39'n'#39'Drop'
    TabOrder = 4
    OnDragDrop = GroupBox1DragDrop
    OnDragOver = GroupBox1DragOver
    OnMouseMove = GroupBox1MouseMove
    object Label2: TLabel
      Left = 26
      Top = 48
      Width = 40
      Height = 13
      Caption = 'Drag me'
      DragMode = dmAutomatic
    end
  end
  object Button4: TButton
    Left = 136
    Top = 120
    Width = 185
    Height = 25
    Caption = 'Create button and set its OnClick'
    TabOrder = 5
    OnClick = Button4Click
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 152
    Width = 417
    Height = 193
    Caption = ' Database '
    TabOrder = 6
    object CheckBox2: TCheckBox
      Left = 16
      Top = 18
      Width = 97
      Height = 17
      Caption = 'Open dataset'
      TabOrder = 0
      OnClick = CheckBox2Click
    end
    object Button2: TButton
      Left = 14
      Top = 161
      Width = 75
      Height = 23
      Caption = 'AddRecord'
      TabOrder = 1
      OnClick = Button2Click
    end
    object DBGrid1: TDBGrid
      Left = 8
      Top = 43
      Width = 401
      Height = 110
      DataSource = DataSource1
      TabOrder = 2
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
    object pnlStatus: TPanel
      Left = 104
      Top = 162
      Width = 305
      Height = 22
      BevelOuter = bvLowered
      Caption = 'pnlStatus'
      TabOrder = 3
    end
  end
  object RegAuto1: TJvFormStorage
    RegPath = 'Software\JVCL\JvInterpreterTest'
    IniFile = '$HOME/.JvInterpreterTest'
    SaveWindowPlace = True
    Left = 240
    Top = 224
  end
  object Table1: TTable
    AfterOpen = Table1ActiveChanged
    AfterClose = Table1ActiveChanged
    DatabaseName = 'DBDemos'
    TableName = 'EMPLOYEE.DB'
    Left = 160
    Top = 216
  end
  object DataSource1: TDataSource
    DataSet = Table1
    Left = 208
    Top = 240
  end
end
