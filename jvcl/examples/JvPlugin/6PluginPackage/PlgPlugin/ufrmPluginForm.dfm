object frmPluginForm: TfrmPluginForm
  Left = 370
  Top = 105
  Width = 272
  Height = 171
  Caption = 'PluginForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 16
    Top = 8
    Width = 233
    Height = 121
    Lines.Strings = (
      'This is a plugin form.'
      ''
      'The way this plugin is implemented now the form '
      'is created on the first event it is needed and it is '
      'released when the main application is closed.')
    TabOrder = 0
  end
end
