object TipOfDayMainForm: TTipOfDayMainForm
  Left = 320
  Top = 278
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'JVCL Tips Demo'
  ClientHeight = 103
  ClientWidth = 285
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 56
    Width = 26
    Height = 13
    Caption = 'Style:'
  end
  object Button1: TButton
    Left = 96
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Load Tips'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Show Tips'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 176
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Save Tips'
    TabOrder = 2
    OnClick = Button3Click
  end
  object cbStyle: TComboBox
    Left = 16
    Top = 72
    Width = 257
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    Items.Strings = (
      'Visual C'
      'Standard')
  end
  object JvTip: TJvTipOfDay
    ButtonNext.Caption = '&Next Tip'
    ButtonNext.Flat = False
    ButtonNext.HotTrack = False
    ButtonNext.HotTrackFont.Charset = DEFAULT_CHARSET
    ButtonNext.HotTrackFont.Color = clWindowText
    ButtonNext.HotTrackFont.Height = -11
    ButtonNext.HotTrackFont.Name = 'MS Sans Serif'
    ButtonNext.HotTrackFont.Style = []
    ButtonNext.HotTrackFontOptions = []
    ButtonNext.ShowHint = False
    ButtonClose.Caption = '&Close'
    ButtonClose.Flat = False
    ButtonClose.HotTrack = False
    ButtonClose.HotTrackFont.Charset = DEFAULT_CHARSET
    ButtonClose.HotTrackFont.Color = clWindowText
    ButtonClose.HotTrackFont.Height = -11
    ButtonClose.HotTrackFont.Name = 'MS Sans Serif'
    ButtonClose.HotTrackFont.Style = []
    ButtonClose.HotTrackFontOptions = []
    ButtonClose.ShowHint = False
    CheckBoxText = '&Show Tips on StartUp'
    DefaultFonts = False
    HeaderFont.Charset = DEFAULT_CHARSET
    HeaderFont.Color = clWindowText
    HeaderFont.Height = -20
    HeaderFont.Name = 'Times New Roman'
    HeaderFont.Style = [fsBold]
    HeaderText = 'Did you know...'
    Options = []
    TipFont.Charset = DEFAULT_CHARSET
    TipFont.Color = clWindowText
    TipFont.Height = -11
    TipFont.Name = 'Arial'
    TipFont.Style = []
    Tips.Strings = (
      
        'The new merged tip component can display in two styles: Visual C' +
        ' and Standard.'
      
        'You can save and load tips from files by using the methods LoadF' +
        'romFile and SaveToFile.'
      
        'You can control detailed aspects of the tip window, like the app' +
        'erance of the buttons, the texts and all the different fonts use' +
        'd in the dialog.'
      
        'You can save and load tips from the registry by just setting a p' +
        'roperty and the component will store the strings in a default lo' +
        'cation for you (Software\JEDI-VCL\TipsStartup + your application' +
        's  name).'
      
        'You can save and load tips from any location (f ex a database) b' +
        'y assigning event handlers to the OnCanShow and OnAfterExecute e' +
        'vents.'
      
        'Each row (or line) of the TStrings property is loaded as a singl' +
        'e tip in the window.'
      
        'Since tips are displayed randomly, make sure your tips doesn'#39't a' +
        'ssume the user has read one of the other tips: each tip should b' +
        'e as self-contained as possible to not confuse the user.'
      
        'To find out if the user wants to see tips the next time he runs ' +
        'the program, check the toShowOnStartUp value in Options.')
    Title = 'Tips and Tricks'
    Left = 104
    Top = 24
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Tips (*.txt)|*.txt'
    Left = 64
    Top = 24
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = 'txt'
    Filter = 'Tips (*.txt)|*.txt'
    Left = 24
    Top = 24
  end
end
