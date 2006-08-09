object SimpleMainForm: TSimpleMainForm
  Left = 313
  Top = 145
  Width = 545
  Height = 542
  Caption = 'JvInspector simple example'
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 250
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object JvInspector1: TJvInspector
    Left = 0
    Top = 0
    Width = 352
    Height = 512
    Align = alClient
    BandWidth = 150
    BevelKind = bkTile
    BevelInner = bvNone
    RelativeDivider = True
    Divider = 50
    ItemHeight = 16
    Painter = JvInspectorBorlandPainter1
    ReadOnly = False
    UseBands = False
    WantTabs = False
  end
  object Panel1: TPanel
    Left = 352
    Top = 0
    Width = 185
    Height = 512
    Align = alRight
    TabOrder = 1
    object Label1: TLabel
      Left = 4
      Top = 8
      Width = 163
      Height = 104
      Caption = 
        'To Use Inspector you need a painter component and the inspector ' +
        'itself. Set the inspector'#39's painter property, then add some item' +
        's. In this example, see AddComponent for the simplest way to ins' +
        'pect a component at runtime.'
      WordWrap = True
    end
  end
  object JvInspectorBorlandPainter1: TJvInspectorBorlandPainter
    Left = 194
    Top = 102
  end
end
