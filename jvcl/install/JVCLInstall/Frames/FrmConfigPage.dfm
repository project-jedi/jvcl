object FrameConfigPage: TFrameConfigPage
  Left = 0
  Top = 0
  Width = 518
  Height = 335
  TabOrder = 0
  object LblBCBGuide: TLabel
    Left = 264
    Top = 312
    Width = 102
    Height = 13
    Cursor = crHandPoint
    Caption = 'BCB installation guide'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    Visible = False
    OnClick = LblBCBGuideClick
  end
  object GroupBoxJvclInc: TGroupBox
    Left = 272
    Top = 8
    Width = 241
    Height = 161
    Caption = ' Global options for all IDEs '
    TabOrder = 1
    object LblDxgettextHomepage: TLabel
      Left = 8
      Top = 74
      Width = 146
      Height = 13
      Cursor = crHandPoint
      Hint = 
        'http://dxgettext.sourceforge.net|Download from <c:blue>http://dx' +
        'gettext.sourceforge.net<c:black>'#13#10'<c:red>The gnugettext.pas unit' +
        ' will be added to the contains list'#13#10'of the JvCore-R package. Th' +
        'at means that you cannot add'#13#10'it to another package.'
      Caption = 'dxgettext &support (not required)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      Visible = False
      OnClick = LblDxgettextHomepageClick
    end
    object CheckBoxXPTheming: TCheckBox
      Left = 8
      Top = 16
      Width = 225
      Height = 17
      Hint = 
        'Activate this if you have Mike Lischke'#39's Theme Manager'#13#10'(<c:navy' +
        '>http://www.lischke-online.de<c:black>) installed and available ' +
        'and'#13#10'you are using <b>Delphi/BCB 6</b> or below. The ThemeManage' +
        'r'#13#10'package must be compiled as "<b>never-build package</b>". The' +
        #13#10'ThemeManager package must be in the BPL directory.'#13#10'For more i' +
        'nformation see <b>readme.htm</b>'#13#10#13#10'<b>For Delphi 7 this option ' +
        'is always enabled even if'#13#10'it is not enabled here.</b>'
      Caption = '&XP Theming (not for Delphi 7)'
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
      Caption = '&Register global design editors'
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
        'Enable this if you want to use the dxgettext'#13#10'(<c:navy>http://dx' +
        'gettext.sourceforge.net<c:black>) translation tool.'#13#10#13#10'<c:red>Th' +
        'e gnugettext.pas unit will be added to the contains list'#13#10'of the' +
        ' JvCore-R package. That means that you cannot add'#13#10'it to another' +
        ' package.'
      Caption = 'dxgettext &support'
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
      Caption = 'Register Jv&Gif for .gif'
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
      Caption = '&Use JVCL for all packages'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = CheckBoxXPThemingClick
    end
    object BtnEditJvclInc: TButton
      Left = 152
      Top = 130
      Width = 81
      Height = 25
      Caption = 'Edit jvcl.inc'
      TabOrder = 5
      OnClick = BtnEditJvclIncClick
    end
  end
  object GroupBoxInstallOptions: TGroupBox
    Left = 8
    Top = 8
    Width = 249
    Height = 321
    Caption = ' Installation options '
    TabOrder = 0
    object LblOptionsFor: TLabel
      Left = 8
      Top = 20
      Width = 54
      Height = 13
      Caption = 'Options for:'
    end
    object CheckBoxDeveloperInstall: TCheckBox
      Left = 16
      Top = 115
      Width = 225
      Height = 17
      Hint = 
        'Activate this option if you are a JVCL developer.'#13#10'This adds the' +
        ' \run, \common and \design directory to the library paths.'
      AllowGrayed = True
      Caption = '&JVCL Developer installation'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxCleanPalettes: TCheckBox
      Left = 16
      Top = 91
      Width = 225
      Height = 17
      Hint = 
        'Remove all JVCL components from the component palettes in'#13#10'order' +
        ' to reinstall in a proper order.'
      AllowGrayed = True
      Caption = 'Clean JVCL component &palettes'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = CheckBoxDeveloperInstallClick
    end
    object ComboBoxTargetIDE: TComboBox
      Left = 72
      Top = 16
      Width = 169
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      TabOrder = 0
      OnChange = ComboBoxTargetIDEChange
      OnDrawItem = ComboBoxTargetIDEDrawItem
    end
    object CheckBoxBuild: TCheckBox
      Left = 8
      Top = 43
      Width = 233
      Height = 17
      Hint = 
        'Check this option if you want to build the packages instead'#13#10'of ' +
        'compiling the modified files.'#13#10#13#10'<c:red><b>WARNING for BCB users' +
        ':</b>'#13#10'This could take up to 10 minutes.'
      AllowGrayed = True
      Caption = 'B&uild packages'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxCompileOnly: TCheckBox
      Left = 8
      Top = 67
      Width = 233
      Height = 17
      Hint = 
        'Check this option if you do not want to register'#13#10'the designtime' +
        ' package into the IDE.'
      AllowGrayed = True
      Caption = 'No IDE registration'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = CheckBoxDeveloperInstallClick
    end
    inline FrameDirEditBrowseBPL: TFrameDirEditBrowse
      Left = 2
      Top = 160
      Width = 239
      Height = 49
      TabOrder = 6
      Visible = False
      inherited LblCaption: TLabel
        Left = 7
        Width = 68
        Caption = 'BP&L Directory:'
      end
      inherited Bevel: TBevel
        Width = 309
        Visible = False
      end
      inherited EditDirectory: TEdit
        Left = 7
        Width = 208
      end
      inherited BtnJCLDirBrowse: TButton
        Left = 216
      end
    end
    inline FrameDirEditBrowseDCP: TFrameDirEditBrowse
      Left = 2
      Top = 208
      Width = 239
      Height = 49
      TabOrder = 7
      Visible = False
      inherited LblCaption: TLabel
        Left = 7
        Width = 70
        Caption = '&DCP Directory:'
      end
      inherited Bevel: TBevel
        Width = 309
        Visible = False
      end
      inherited EditDirectory: TEdit
        Left = 7
        Width = 208
      end
      inherited BtnJCLDirBrowse: TButton
        Left = 216
      end
    end
    inline FrameDirEditBrowseHPP: TFrameDirEditBrowse
      Left = 2
      Top = 264
      Width = 239
      Height = 49
      TabOrder = 8
      Visible = False
      inherited LblCaption: TLabel
        Left = 7
        Width = 70
        Caption = '&HPP Directory:'
        ParentShowHint = False
      end
      inherited Bevel: TBevel
        Width = 309
        Visible = False
      end
      inherited EditDirectory: TEdit
        Left = 7
        Width = 208
        Hint = 
          'The HPP directory specifies where the generated .hpp files'#13#10'shou' +
          'ld go. If this field is empty the generated .hpp files are'#13#10'writ' +
          'ten to the directory where the pascal source file is.'
        ParentShowHint = False
        ShowHint = True
      end
      inherited BtnJCLDirBrowse: TButton
        Left = 216
      end
    end
    object CheckBoxGenerateMapFiles: TCheckBox
      Left = 8
      Top = 139
      Width = 233
      Height = 17
      Hint = 
        'Activate this option if the installer should generate'#13#10'detailed ' +
        'map-files for each package.'#13#10#13#10'Map files are used for debug purp' +
        'oses, e.g. JclDebug'#13#10'could use them.'
      AllowGrayed = True
      Caption = 'Genera&te Map files'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = CheckBoxDeveloperInstallClick
    end
  end
  object CheckBoxCompileJclDcp: TCheckBox
    Left = 272
    Top = 176
    Width = 241
    Height = 17
    Caption = 'Co&mpile JCL .dcp files if necessary'
    TabOrder = 2
    OnClick = CheckBoxCompileJclDcpClick
  end
  object CheckBoxVerbose: TCheckBox
    Left = 272
    Top = 200
    Width = 241
    Height = 17
    Caption = '&Verbose compiler output'
    TabOrder = 3
    OnClick = CheckBoxCompileJclDcpClick
  end
  object ImageListTargets: TImageList
    Left = 200
    Top = 16
  end
end
