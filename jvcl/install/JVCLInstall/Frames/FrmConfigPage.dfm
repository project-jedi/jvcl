object FrameConfigPage: TFrameConfigPage
  Left = 0
  Top = 0
  Width = 518
  Height = 397
  TabOrder = 0
  TabStop = True
  DesignSize = (
    518
    397)
  object LblBCBGuide: TLabel
    Left = 279
    Top = 379
    Width = 102
    Height = 13
    Cursor = crHandPoint
    Hint = 
      'Open the default web browser to show the C++Builder Installation' +
      ' Guide.'
    Caption = 'BCB installation guide'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    Visible = False
    OnClick = LblBCBGuideClick
  end
  object LblEnvPathWarning: TLabel
    Left = 279
    Top = 257
    Width = 236
    Height = 98
    AutoSize = False
    Caption = 
      'The BPL directory is not in the PATH environment variable. If yo' +
      'u do not add the directory to the PATH environment variable, Del' +
      'phi/BCB will not be able to find the runtime packages and shows ' +
      'you lots of error messages like: "Cannot load package JvCoreD10R' +
      '.bpl"'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object GroupBoxJvclInc: TGroupBox
    Left = 279
    Top = 31
    Width = 234
    Height = 130
    Caption = ' JVCL Options  '
    TabOrder = 1
    object CheckBoxXPTheming: TCheckBox
      Left = 8
      Top = 16
      Width = 223
      Height = 17
      Hint = 
        'Activate this if you have Mike Lischke'#39's Theme Manager'#13#10'(<c:navy' +
        '>http://www.lischke-online.de<c:black>) installed and available ' +
        'and'#13#10'you are using <b>Delphi/BCB 6</b> or below. The ThemeManage' +
        'r'#13#10'package must be compiled as "<b>never-build package</b>".'#13#10'Th' +
        'e ThemeManager package must be in the BPL directory.'#13#10'For more i' +
        'nformation see <b>readme.htm</b>'#13#10#13#10'If you activate the XP Themi' +
        'ng all your installed'#13#10'Delphi/C++Builder versions must have the ' +
        'ThemeManager'#13#10'installed (not for Delphi 7)'#13#10#13#10'<b>For Delphi 7 th' +
        'is option is always enabled even if'#13#10'it is not enabled here.</b>'
      Caption = '&XP Theming (for Delphi 6 or less)'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = CheckBoxXPThemingClick
    end
    object CheckBoxRegisterGlobalDesignEditors: TCheckBox
      Left = 8
      Top = 39
      Width = 223
      Height = 17
      Hint = 
        'Enable this option if you want to register property and componen' +
        't'#13#10'editors included in JVCL (for non-JVCL components as well).'
      Caption = '&Register global design editors'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = CheckBoxXPThemingClick
    end
    object CheckBoxDxgettextSupport: TCheckBox
      Left = 8
      Top = 62
      Width = 223
      Height = 17
      Hint = 
        'Enable this option if you want to use the dxgettext'#13#10'(<c:navy>ht' +
        'tp://dxgettext.sourceforge.net<c:black>) translation tool.'#13#10#13#10'<c' +
        ':red>The JVCL 3 ships it'#39's own gnugettext.pas unit called'#13#10'JvGnu' +
        'gettext. This prevents package unit conflicts and'#13#10'problems with' +
        ' wrong versions.'
      Caption = 'dxgettext &support'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = CheckBoxXPThemingClick
    end
    object CheckBoxRegisterJvGif: TCheckBox
      Left = 8
      Top = 85
      Width = 223
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
    object CheckBoxUnitVersioning: TCheckBox
      Left = 8
      Top = 108
      Width = 223
      Height = 17
      Hint = 
        'Enable this option if you want that every JVCL unit should suppo' +
        'rt'#13#10'UnitVersioning.'#13#10'UnitVersioning allows you the obtain the re' +
        'vision, date and filename'#13#10'of each unit at runtime.'
      Caption = 'Unit Versioning'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = CheckBoxXPThemingClick
    end
  end
  object GroupBoxInstallOptions: TGroupBox
    Left = 8
    Top = 31
    Width = 265
    Height = 361
    Caption = ' Installation options '
    TabOrder = 0
    object CheckBoxDeveloperInstall: TCheckBox
      Left = 8
      Top = 85
      Width = 254
      Height = 17
      Hint = 
        'Activate this option if you are a JVCL developer.'#13#10'This adds the' +
        ' \run, \common, \design, \qrun,'#13#10'\qcommon and \qdesign directori' +
        'es to the library'#13#10'paths.'
      AllowGrayed = True
      Caption = '&JVCL Developer installation'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 3
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxCleanPalettes: TCheckBox
      Left = 24
      Top = 59
      Width = 238
      Height = 17
      Hint = 
        'Remove all JVCL components from the component palettes in'#13#10'order' +
        ' to reinstall in a proper order.'
      AllowGrayed = True
      Caption = 'Clean JVCL component &palettes'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxBuild: TCheckBox
      Left = 8
      Top = 16
      Width = 254
      Height = 17
      Hint = 
        'Check this option if you want to build the packages instead'#13#10'of ' +
        'compiling the modified files.'#13#10#13#10'<c:red><b>WARNING for BCB users' +
        ':</b>'#13#10'This could take up to 10 minutes.'
      AllowGrayed = True
      Caption = 'B&uild packages'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxIDERegister: TCheckBox
      Left = 8
      Top = 39
      Width = 254
      Height = 17
      Hint = 
        'Check this option if you want to register'#13#10'the designtime packag' +
        'es into the IDE.'
      AllowGrayed = True
      Caption = '&IDE registration'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = CheckBoxDeveloperInstallClick
    end
    inline FrameDirEditBrowseBPL: TFrameDirEditBrowse
      Left = 2
      Top = 203
      Width = 261
      Height = 49
      TabOrder = 9
      TabStop = True
      Visible = False
      ExplicitLeft = 2
      ExplicitTop = 203
      ExplicitWidth = 261
      ExplicitHeight = 49
      inherited LblCaption: TLabel
        Left = 7
        Width = 68
        Caption = 'BP&L Directory:'
        ExplicitLeft = 7
        ExplicitWidth = 68
      end
      inherited Bevel: TBevel
        Width = 261
        Visible = False
        ExplicitWidth = 261
      end
      inherited EditDirectory: TEdit
        Left = 7
        Width = 227
        OnChange = FrameDirEditBrowseBPLEditDirectoryChange
        ExplicitLeft = 7
        ExplicitWidth = 227
      end
      inherited BtnDirBrowse: TButton
        Left = 234
        ExplicitLeft = 234
      end
    end
    inline FrameDirEditBrowseHPP: TFrameDirEditBrowse
      Left = 2
      Top = 258
      Width = 261
      Height = 49
      TabOrder = 10
      TabStop = True
      Visible = False
      ExplicitLeft = 2
      ExplicitTop = 258
      ExplicitWidth = 261
      ExplicitHeight = 49
      inherited LblCaption: TLabel
        Left = 7
        Width = 70
        Caption = '&HPP Directory:'
        ParentShowHint = False
        ExplicitLeft = 7
        ExplicitWidth = 70
      end
      inherited Bevel: TBevel
        Width = 261
        Visible = False
        ExplicitWidth = 261
      end
      inherited EditDirectory: TEdit
        Left = 7
        Width = 227
        Hint = 
          'The HPP directory specifies where the generated .hpp files'#13#10'shou' +
          'ld go. If this field is empty the generated .hpp files are'#13#10'writ' +
          'ten to the directory where the pascal source file is.'
        ExplicitLeft = 7
        ExplicitWidth = 227
      end
      inherited BtnDirBrowse: TButton
        Left = 234
        OnClick = FrameDirEditBrowseHPPBtnJCLDirBrowseClick
        ExplicitLeft = 234
      end
    end
    object CheckBoxGenerateMapFiles: TCheckBox
      Left = 8
      Top = 131
      Width = 254
      Height = 17
      Hint = 
        'Activate this option if the installer should generate'#13#10'detailed ' +
        'map-files for each package.The .map files'#13#10'are written only for ' +
        'newly compiled packages.'#13#10#13#10'Map files are used for debug purpose' +
        's, e.g. JclDebug'#13#10'could use them.'
      AllowGrayed = True
      Caption = 'Genera&te Map files'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxDebugUnits: TCheckBox
      Left = 24
      Top = 105
      Width = 238
      Height = 17
      Hint = 
        'Activate this option if you also want to compile debug units.'#13#10'J' +
        'VCL Developer installations units do not need this because'#13#10'they' +
        ' are compiled with debug information included.'
      AllowGrayed = True
      Caption = 'Compile debug units'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 4
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxLinkMapFiles: TCheckBox
      Left = 24
      Top = 151
      Width = 238
      Height = 17
      Hint = 
        'Activate this option if the installer should link detailled'#13#10'map' +
        ' information as a resource of the binary it is describing.'#13#10#13#10'Th' +
        'e redistribution of the binary and Map file is'#13#10'simplified to th' +
        'e redistribution of the binary.'#13#10#13#10'JclDebug can use these inform' +
        'ation.'
      Caption = '&Link Map files in binaries'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 6
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxDeleteMapFiles: TCheckBox
      Left = 24
      Top = 185
      Width = 201
      Height = 17
      Hint = 
        'Activate this option if the installer should delete'#13#10'detailed ma' +
        'p-files after they are linked in the binary'#13#10'or the jdbg files w' +
        'ere created.'
      Caption = '&Delete after linking/compression'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 8
      OnClick = CheckBoxDeveloperInstallClick
    end
    object CheckBoxCreateJdbgFiles: TCheckBox
      Left = 24
      Top = 168
      Width = 238
      Height = 17
      Hint = 
        'Activate this option if the installer should compress detailled'#13 +
        #10'map information to jdbg files.'#13#10#13#10'JclDebug can use these inform' +
        'ation.'
      Caption = 'Compress Map files to .jdbg'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 7
      OnClick = CheckBoxDeveloperInstallClick
    end
  end
  object CheckBoxVerbose: TCheckBox
    Left = 279
    Top = 173
    Width = 223
    Height = 17
    Hint = 
      'Activate this option if you want to see a more detailed compiler' +
      ' log on'#13#10'the installation progress page.'
    Caption = '&Verbose compiler output'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = CheckBoxCompileJclDcpClick
  end
  object CheckBoxIgnoreMakeErrors: TCheckBox
    Left = 279
    Top = 196
    Width = 223
    Height = 17
    Hint = 
      'Activate this option if the installer should not stop when an er' +
      'ror occurred while'#13#10'compiling the packages.'
    Caption = 'D&o not stop on errors'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 3
    OnClick = CheckBoxCompileJclDcpClick
  end
  object ComboBoxTargetIDE: TComboBox
    Left = 80
    Top = 8
    Width = 298
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    OnChange = ComboBoxTargetIDEChange
    OnDrawItem = ComboBoxTargetIDEDrawItem
  end
  object BtnEditJvclInc: TButton
    Left = 384
    Top = 7
    Width = 129
    Height = 25
    Hint = 'Edit all compile time conditions that are in the jvcl.inc file.'
    Anchors = [akTop, akRight]
    Caption = 'Edit jvcl%s%d.inc'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    OnClick = BtnEditJvclIncClick
  end
  object PanelBk: TPanel
    Left = 0
    Top = 8
    Width = 78
    Height = 22
    BevelOuter = bvNone
    TabOrder = 5
    object LblOptionsFor: TLabel
      Left = 8
      Top = 4
      Width = 58
      Height = 13
      Caption = '&Options for:'
      FocusControl = ComboBoxTargetIDE
    end
  end
  object CheckBoxAddBplDirToPath: TCheckBox
    Left = 279
    Top = 234
    Width = 220
    Height = 17
    Caption = 'Add BPL directory to PATH'
    TabOrder = 4
    Visible = False
    OnClick = CheckBoxDeveloperInstallClick
  end
  object ImageListTargets: TImageList
    Left = 200
    Top = 16
  end
end
