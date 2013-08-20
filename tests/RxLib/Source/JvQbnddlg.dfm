object QueryParamsDialog: TJvQueryParamsDialog
  Left = 210
  Top = 119
  ActiveControl = ParamList
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Query parameters'
  ClientHeight = 183
  ClientWidth = 354
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 9
    Top = 3
    Width = 336
    Height = 143
    Caption = 'Define Parameters'
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 80
      Height = 13
      Caption = '&Parameter name:'
      FocusControl = ParamList
    end
    object Label2: TLabel
      Left = 143
      Top = 66
      Width = 30
      Height = 13
      Caption = '&Value:'
      FocusControl = ParamValue
    end
    object Label3: TLabel
      Left = 143
      Top = 40
      Width = 49
      Height = 13
      Caption = '&Data type:'
      FocusControl = TypeList
    end
    object ParamValue: TEdit
      Left = 208
      Top = 62
      Width = 121
      Height = 21
      TabOrder = 2
      OnExit = ParamValueExit
    end
    object NullValue: TCheckBox
      Left = 143
      Top = 112
      Width = 82
      Height = 17
      Caption = '&Null Value'
      TabOrder = 3
      OnClick = NullValueClick
    end
    object TypeList: TComboBox
      Left = 208
      Top = 36
      Width = 121
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 1
      OnChange = TypeListChange
    end
    object ParamList: TListBox
      Left = 8
      Top = 36
      Width = 121
      Height = 93
      ItemHeight = 13
      Sorted = True
      TabOrder = 0
      OnClick = ParamListChange
    end
  end
  object OkBtn: TButton
    Left = 54
    Top = 153
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 139
    Top = 153
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object HelpBtn: TButton
    Left = 224
    Top = 153
    Width = 75
    Height = 25
    Caption = '&Help'
    TabOrder = 3
    OnClick = HelpBtnClick
  end
end
