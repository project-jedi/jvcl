object Form1: TForm1
  Left = 282
  Top = 237
  Width = 686
  Height = 501
  Anchors = [akLeft, akTop, akRight, akBottom]
  Caption = 'Small and dirty installer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object JvGradient1: TJvGradient
    Left = 0
    Top = 0
    Width = 678
    Height = 474
    Align = alClient
    ShowHint = True
    ParentShowHint = False
  end
  object JvInstaller1: TJvInstaller
    Options.ProgramName = 'Your Program Name'
    Options.Maximized = True
    OnCancel = JvInstaller1Cancel
    OnLoaded = JvInstaller1Loaded
    Left = 84
    Top = 88
  end
  object JvWelcome1: TJvWelcome
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Enabled = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvAgreement1
    Left = 116
    Top = 156
  end
  object JvUsernameSerial1: TJvUsernameSerial
    OnNextPage = JvUsernameSerial1NextPage
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvFinishInstall1
    PreviousPage = JvInstallProgress1
    Left = 400
    Top = 156
  end
  object JvInstallProgress1: TJvInstallProgress
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Enabled = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Enabled = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvUsernameSerial1
    PreviousPage = JvCompoInstall1
    Left = 368
    Top = 156
  end
  object JvCompoInstall1: TJvCompoInstall
    OnNextPageShown = JvInstallProgress1NextPageShown
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvInstallConfirm1
    PreviousPage = JvInstallType1
    Options.Items.Data = {
      6A0000000300000000000000FFFFFFFFFFFFFFFF00000000000000000F466972
      737420636F6D706F6E656E7400000000FFFFFFFFFFFFFFFF0000000000000000
      105365636F6E6420636F6D706F6E656E7400000000FFFFFFFFFFFFFFFF000000
      00000000000448656C70}
    Options.AtLeastOne = False
    Left = 300
    Top = 156
  end
  object JvInstallType1: TJvInstallType
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvCompoInstall1
    PreviousPage = JvSelectGroup1
    Options.ItemIndex = 0
    Options.Items.Strings = (
      'Typical'
      'Custom'
      'Full'
      'Everything you want here !')
    Left = 270
    Top = 156
  end
  object JvSelectGroup1: TJvSelectGroup
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvInstallType1
    PreviousPage = JvSelectDir1
    Options.Group = 'TestGroup'
    Left = 238
    Top = 156
  end
  object JvSelectDir1: TJvSelectDir
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvSelectGroup1
    PreviousPage = JvReadme1
    Left = 208
    Top = 156
  end
  object JvReadme1: TJvReadme
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvSelectDir1
    PreviousPage = JvAgreement1
    Options.Text.Strings = (
      'This is the text of the readme')
    Left = 176
    Top = 156
  end
  object JvAgreement1: TJvAgreement
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvReadme1
    PreviousPage = JvWelcome1
    Options.Item.Strings = (
      'This is the text of the agreement !')
    Left = 146
    Top = 156
  end
  object JvFinishInstall1: TJvFinishInstall
    OnNext = JvFinishInstall1Next
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    PreviousPage = JvUsernameSerial1
    Options.CheckBox1.Visible = True
    Options.CheckBox1.Checked = True
    Options.CheckBox1.Text = 'First checkbox'
    Options.CheckBox2.Visible = True
    Options.CheckBox2.Checked = True
    Options.CheckBox2.Text = 'Hello world ;)'
    Options.CheckBox3.Visible = True
    Options.CheckBox3.Checked = True
    Options.CheckBox3.Text = 'Launch the program'
    Options.CheckBox4.Visible = False
    Options.CheckBox4.Checked = False
    Options.CheckBox5.Visible = False
    Options.CheckBox5.Checked = False
    Left = 432
    Top = 156
  end
  object JvConfirmCancel1: TJvConfirmCancel
    Installer = JvInstaller1
    OnExit = JvConfirmCancel1Exit
    Options.Resume.Caption = '&Resume'
    Options.Resume.HotTrackFont.Charset = DEFAULT_CHARSET
    Options.Resume.HotTrackFont.Color = clWindowText
    Options.Resume.HotTrackFont.Height = -11
    Options.Resume.HotTrackFont.Name = 'MS Sans Serif'
    Options.Resume.HotTrackFont.Style = []
    Options.Resume.Hint = 'Click to resume'
    Options.Resume.ShowHint = False
    Options.Exit.Caption = '&Exit'
    Options.Exit.HotTrackFont.Charset = DEFAULT_CHARSET
    Options.Exit.HotTrackFont.Color = clWindowText
    Options.Exit.HotTrackFont.Height = -11
    Options.Exit.HotTrackFont.Name = 'MS Sans Serif'
    Options.Exit.HotTrackFont.Style = []
    Options.Exit.Hint = 'Click to exit'
    Options.Exit.ShowHint = False
    Left = 118
    Top = 88
  end
  object JvInstallConfirm1: TJvInstallConfirm
    OnNext = JvInstallProgress1NextPageShown
    Buttons.Cancel.Caption = '&Cancel'
    Buttons.Cancel.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Cancel.HotTrackFont.Color = clWindowText
    Buttons.Cancel.HotTrackFont.Height = -11
    Buttons.Cancel.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Cancel.HotTrackFont.Style = []
    Buttons.Cancel.ShowHint = False
    Buttons.Next.Caption = '&Next'
    Buttons.Next.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Next.HotTrackFont.Color = clWindowText
    Buttons.Next.HotTrackFont.Height = -11
    Buttons.Next.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Next.HotTrackFont.Style = []
    Buttons.Next.ShowHint = False
    Buttons.Previous.Caption = '&Previous'
    Buttons.Previous.HotTrackFont.Charset = DEFAULT_CHARSET
    Buttons.Previous.HotTrackFont.Color = clWindowText
    Buttons.Previous.HotTrackFont.Height = -11
    Buttons.Previous.HotTrackFont.Name = 'MS Sans Serif'
    Buttons.Previous.HotTrackFont.Style = []
    Buttons.Previous.ShowHint = False
    Installer = JvInstaller1
    NextPage = JvInstallProgress1
    PreviousPage = JvCompoInstall1
    Left = 332
    Top = 156
  end
end
