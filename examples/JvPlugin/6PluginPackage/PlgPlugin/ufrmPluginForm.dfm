object frmPluginForm: TfrmPluginForm
  Left = 370
  Top = 105
  Width = 272
  Height = 223
  Caption = 'PluginForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    264
    196)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 16
    Top = 8
    Width = 233
    Height = 129
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'This is a plugin form.'
      ''
      'The way this plugin is implemented, the plugin '
      'form is created when needed and released '
      'when the main application is closed.')
    TabOrder = 0
  end
  object Button1: TButton
    Left = 16
    Top = 152
    Width = 233
    Height = 25
    Anchors = [akLeft, akRight, akBottom]
    Caption = 'Call DoSomethingSpecial in Main App'
    TabOrder = 1
    OnClick = Button1Click
  end
end
