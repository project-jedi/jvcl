object RaInterpreterEndUserMainForm: TRaInterpreterEndUserMainForm
  Left = 352
  Top = 106
  Width = 585
  Height = 420
  ActiveControl = Panel1
  Caption = 'JvI2 plug-in reports end user demo'
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 577
    Height = 41
    Align = alTop
    TabOrder = 0
    object GradButton1: TButton
      Left = 264
      Top = 8
      Width = 100
      Height = 25
      Caption = 'Create report'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = GradButton1Click
    end
    object DBNavigator: TDBNavigator
      Left = 8
      Top = 8
      Width = 240
      Height = 25
      DataSource = DMRaIntrEndUsr.DataSource1
      Ctl3D = False
      ParentCtl3D = False
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 376
    Height = 349
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 4
    Caption = 'Panel2'
    TabOrder = 1
    object DBGrid1: TDBGrid
      Left = 6
      Top = 6
      Width = 364
      Height = 337
      Align = alClient
      BorderStyle = bsNone
      DataSource = DMRaIntrEndUsr.DataSource1
      TabOrder = 0
      TitleFont.Charset = DEFAULT_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'MS Sans Serif'
      TitleFont.Style = []
    end
  end
  object Panel3: TPanel
    Left = 376
    Top = 41
    Width = 201
    Height = 349
    Align = alRight
    BorderWidth = 3
    Caption = 'Panel3'
    TabOrder = 2
    object Memo1: TMemo
      Left = 4
      Top = 4
      Width = 193
      Height = 341
      Align = alClient
      Lines.Strings = (
        'This is normal Delphi'
        'form. Here user edit data.'
        ''
        'Click '#39'Create Report'#39' to see another'
        'from that lists all available reports.'
        'It'#39's normal Delphi form too.'
        'But list of reports filled directly'
        'from disk from folder '#39'Reports'#39'.'
        ''
        'Each report in this folder'
        'consists from two files: '#39'*.dfm'#39
        'and assotiated '#39'*.pas'#39'.'
        ''
        'List of reports filled from ini-file.'
        'Moreover all existing ini-files'
        'combines in one. This alows you'
        'distribute new reports with it own'
        'ini-file (.dfm + .pas + .ini).'
        'When user copied this'
        'three files into '#39'Reports'#39' folder'
        'new report automatically added to'
        'report list.'
        ''
        'Click '#39'Create Report'#39'. You must see'
        'two reports. Close '#39'Reports'#39' form'
        'and move all files from folder '#39'new'#39
        'to folder '#39'Reports'#39'.'
        'Click '#39'Create Report'#39' again and'
        'now you can see three reports'
        'in list.'
        ''
        'That'#39's all. ')
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object RegAuto1: TJvFormStorage
    StoredValues = <>
    Left = 172
    Top = 164
  end
end
