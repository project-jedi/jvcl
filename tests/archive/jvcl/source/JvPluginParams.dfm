object frmPluginParams: TfrmPluginParams
  Left = 424
  Top = 232
  Width = 343
  Height = 247
  BorderIcons = [biSystemMenu]
  Caption = 'Setup Plugin Parameters'
  Color = clBtnFace
  Constraints.MaxHeight = 247
  Constraints.MinHeight = 247
  Constraints.MinWidth = 343
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblCreateInfo: TLabel
    Left = 5
    Top = 110
    Width = 325
    Height = 71
    Anchors = [akLeft, akTop, akRight, akBottom]
    AutoSize = False
    WordWrap = True
  end
  object butOK: TButton
    Left = 175
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object butCancel: TButton
    Left = 255
    Top = 190
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object gbPluginSettings: TGroupBox
    Left = 5
    Top = 5
    Width = 325
    Height = 96
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Plugin settings '
    TabOrder = 2
    object edName: TEdit
      Left = 10
      Top = 20
      Width = 305
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'PluginName'
      OnChange = SettingsChanged
    end
    object rbPackage: TRadioButton
      Left = 10
      Top = 50
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Package plugin'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = SettingsChanged
    end
    object rbDLL: TRadioButton
      Left = 10
      Top = 70
      Width = 305
      Height = 17
      Anchors = [akLeft, akTop, akRight]
      Caption = 'DLL plugin (old style)'
      TabOrder = 2
      OnClick = SettingsChanged
    end
  end
end
