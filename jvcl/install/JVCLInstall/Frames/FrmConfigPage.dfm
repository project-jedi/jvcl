object FrameConfigPage: TFrameConfigPage
  Left = 0
  Top = 0
  Width = 518
  Height = 335
  TabOrder = 0
  object GroupBoxJvclInc: TGroupBox
    Left = 272
    Top = 8
    Width = 241
    Height = 153
    Caption = ' Global options for all IDEs '
    TabOrder = 1
    object LblDxgettextHomepage: TLabel
      Left = 8
      Top = 74
      Width = 176
      Height = 13
      Cursor = crHandPoint
      Hint = 'http://dxgettext.sourceforge.net'
      Caption = 'LblDxgettextHomepage (not required)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      Visible = False
    end
    object CheckBoxXPTheming: TCheckBox
      Left = 8
      Top = 16
      Width = 225
      Height = 17
      Hint = 
        'Activate this if you have Mike Lischke'#39's Theme Manager'#13#10'(<c:blue' +
        '>http://www.lischke-online.de<c:black>) installed and available ' +
        'and'#13#10'you are using <b>Delphi/BCB 6</b> or below. The ThemeManage' +
        'r'#13#10'package must be compiled as "<b>never-build package</b>".'#13#10'Fo' +
        'r more information see readme.htm'#13#10#13#10'<b>For Delphi 7 this option' +
        ' is allways enabled even if'#13#10'it is not enabled here.</b>'
      Caption = 'XP Theming (not for Delphi 7)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = CheckBoxXPThemingClick
    end
    object CheckBoxRegisterGlobalDesignEditors: TCheckBox
      Left = 8
      Top = 40
      Width = 225
      Height = 17
      Hint = 
        'Enable this if you want to register property and component'#13#10'edit' +
        'ors included in JVCL (for non-JVCL components as well).'
      Caption = 'Register global design editors'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = CheckBoxXPThemingClick
    end
    object CheckBoxDxgettextSupport: TCheckBox
      Left = 8
      Top = 64
      Width = 225
      Height = 17
      Hint = 
        'Enable this if you want to use the dxgettext'#13#10'(<c:blue>http://dx' +
        'gettext.sourceforge.net<c:black>) translation tool.'
      Caption = 'dxgettext support'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = CheckBoxXPThemingClick
    end
    object CheckBoxRegisterJvGif: TCheckBox
      Left = 8
      Top = 88
      Width = 225
      Height = 17
      Hint = 
        'Enable this option if you want to register the GIF image'#13#10'Graphi' +
        'c Extensions for Open/Save dialogs.'
      Caption = 'Register JvGif for .gif'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = CheckBoxXPThemingClick
    end
    object CheckBoxUseJVCL: TCheckBox
      Left = 8
      Top = 112
      Width = 225
      Height = 17
      Hint = 
        'Disable this if you want some packages that are not yet'#13#10'complet' +
        'ely integrated to JVCL to be stand alone packages.'
      Caption = 'Use JVCL for all packages'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = CheckBoxXPThemingClick
    end
  end
  object GroupBoxInstallOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 249
    Height = 153
    Caption = ' Installation options '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 54
      Height = 13
      Caption = 'Options for:'
    end
    object CheckBoxDeveloperInstall: TCheckBox
      Left = 8
      Top = 56
      Width = 233
      Height = 17
      Hint = 
        'Activate this option if you are a JVCL developer.'#13#10'This adds the' +
        ' \run, \common and \design directory to the library paths.'
      AllowGrayed = True
      Caption = 'JVCL Developer installation'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxCleanPalettes: TCheckBox
      Left = 8
      Top = 80
      Width = 233
      Height = 17
      Hint = 
        'Remove all JVCL components from the component palettes in'#13#10'order' +
        ' to reinstall in a proper order.'
      AllowGrayed = True
      Caption = 'Clean JVCL component palettes'
      Checked = True
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 1
      OnClick = CheckBoxDeveloperInstallClick
    end
    object ComboBoxTargetIDE: TComboBox
      Left = 72
      Top = 16
      Width = 169
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 2
      OnChange = ComboBoxTargetIDEChange
      OnDrawItem = ComboBoxTargetIDEDrawItem
    end
    object CheckBoxBuild: TCheckBox
      Left = 8
      Top = 104
      Width = 233
      Height = 17
      Hint = 
        'Check this option if you want to build the packages instead'#13#10'of ' +
        'compiling the modified files.'
      AllowGrayed = True
      Caption = 'Build packages'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxCompileOnly: TCheckBox
      Left = 8
      Top = 128
      Width = 233
      Height = 17
      Hint = 
        'Check this option if you do not want to register'#13#10'the designtime' +
        ' package into the IDE.'
      AllowGrayed = True
      Caption = 'Compile only'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = CheckBoxDeveloperInstallClick
    end
  end
  object ScrollBoxBCB: TScrollBox
    Left = 8
    Top = 168
    Width = 505
    Height = 161
    HorzScrollBar.Tracking = True
    VertScrollBar.Tracking = True
    BorderStyle = bsNone
    TabOrder = 2
  end
  object ImageListTargets: TImageList
    Left = 224
    Top = 8
  end
end
