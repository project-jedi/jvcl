object frmPluginParams: TfrmPluginParams
  Left = 222
  Top = 120
  Width = 335
  Height = 266
  Caption = 'Setup Plugin Parameters'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 72
    Height = 13
    Caption = 'Name of Plugin'
  end
  object Label2: TLabel
    Left = 16
    Top = 48
    Width = 291
    Height = 52
    Caption = 
      'A Plugin named Test would have '#13#10#13#10'*) a Project-file called PlgT' +
      'est.dpk'#13#10'*) a Unit called PluginTest.pas containing the Object T' +
      'plgTest'
  end
  object radPluginType: TRadioGroup
    Left = 48
    Top = 120
    Width = 201
    Height = 57
    Caption = 'Plugin-Type'
    ItemIndex = 1
    Items.Strings = (
      'DLL-Plugin (old style)'
      'Package-Plugin')
    TabOrder = 0
  end
  object ediPluginName: TEdit
    Left = 96
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'PluginName'
  end
  object butOK: TButton
    Left = 80
    Top = 200
    Width = 75
    Height = 25
    Caption = 'OK'
    ModalResult = 1
    TabOrder = 2
  end
  object butCancel: TButton
    Left = 168
    Top = 200
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
end
