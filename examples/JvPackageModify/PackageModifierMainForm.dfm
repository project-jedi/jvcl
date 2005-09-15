object PackageModifierMainFrm: TPackageModifierMainFrm
  Left = 354
  Top = 190
  Width = 432
  Height = 484
  Caption = 'Package modifier'
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 430
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 406
    Height = 389
    ActivePage = tabOptions
    Anchors = [akLeft, akTop, akRight, akBottom]
    object tabOptions: TTabSheet
      Caption = 'Options'
      object GroupBox1: TGroupBox
        Left = 16
        Top = 16
        Width = 185
        Height = 73
        Caption = ' Build options '
        TabOrder = 0
        object rbImplicitBuildOff: TRadioButton
          Left = 16
          Top = 46
          Width = 113
          Height = 17
          Caption = 'Explicit rebuild'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rbImplicitBuildOn: TRadioButton
          Left = 16
          Top = 24
          Width = 113
          Height = 17
          Caption = 'Rebuild as needed'
          TabOrder = 1
        end
      end
      object GroupBox2: TGroupBox
        Left = 16
        Top = 96
        Width = 185
        Height = 81
        Caption = ' Code generation '
        TabOrder = 1
        object ckhOptimization: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Optimization'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object chkStackFrames: TCheckBox
          Left = 16
          Top = 40
          Width = 145
          Height = 17
          Caption = 'Stack frames'
          TabOrder = 1
        end
        object chkSafeDivide: TCheckBox
          Left = 16
          Top = 56
          Width = 129
          Height = 17
          Caption = 'Pentium safe FDIV'
          TabOrder = 2
        end
      end
      object GroupBox3: TGroupBox
        Left = 16
        Top = 184
        Width = 185
        Height = 153
        Caption = ' Syntax options '
        TabOrder = 2
        object chkVarStringChecks: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Strict var-strings'
          Checked = True
          State = cbChecked
          TabOrder = 0
        end
        object chkBoolEval: TCheckBox
          Left = 16
          Top = 40
          Width = 145
          Height = 17
          Caption = 'Complete boolean eval'
          TabOrder = 1
        end
        object chkExtendedSyntax: TCheckBox
          Left = 16
          Top = 56
          Width = 129
          Height = 17
          Caption = 'Extended syntax'
          Checked = True
          State = cbChecked
          TabOrder = 2
        end
        object chkTypedAddress: TCheckBox
          Left = 16
          Top = 72
          Width = 137
          Height = 17
          Caption = 'Typed @ operator'
          TabOrder = 3
        end
        object chkOpenStrings: TCheckBox
          Left = 16
          Top = 88
          Width = 137
          Height = 17
          Caption = 'Open parameters'
          Checked = True
          State = cbChecked
          TabOrder = 4
        end
        object chkLongStrings: TCheckBox
          Left = 16
          Top = 104
          Width = 97
          Height = 17
          Caption = 'Huge strings'
          Checked = True
          State = cbChecked
          TabOrder = 5
        end
        object chkWriteableConst: TCheckBox
          Left = 16
          Top = 120
          Width = 153
          Height = 17
          Caption = 'Assignable typed constants'
          Checked = True
          State = cbChecked
          TabOrder = 6
        end
      end
      object GroupBox4: TGroupBox
        Left = 208
        Top = 16
        Width = 185
        Height = 81
        Caption = ' Runtime errors '
        TabOrder = 3
        object chkOverflowChecks: TCheckBox
          Left = 16
          Top = 56
          Width = 129
          Height = 17
          Caption = 'Overflow checking'
          TabOrder = 2
        end
        object chkIOChecks: TCheckBox
          Left = 16
          Top = 40
          Width = 145
          Height = 17
          Caption = 'I/O checking'
          Checked = True
          State = cbChecked
          TabOrder = 1
        end
        object chkRangeChecks: TCheckBox
          Left = 16
          Top = 24
          Width = 97
          Height = 17
          Caption = 'Range checking'
          TabOrder = 0
        end
      end
      object GroupBox5: TGroupBox
        Left = 208
        Top = 104
        Width = 185
        Height = 97
        Caption = ' Debugging '
        TabOrder = 4
        object chkDebugInfo: TCheckBox
          Left = 16
          Top = 24
          Width = 113
          Height = 17
          Caption = 'Debug information'
          TabOrder = 0
        end
        object chkLocalSymbols: TCheckBox
          Left = 16
          Top = 40
          Width = 145
          Height = 17
          Caption = 'Local symbols'
          TabOrder = 1
        end
        object chkReferenceInfo: TCheckBox
          Left = 16
          Top = 56
          Width = 129
          Height = 17
          Caption = 'Reference info'
          TabOrder = 2
        end
        object chkAssertions: TCheckBox
          Left = 16
          Top = 72
          Width = 145
          Height = 17
          Caption = 'Assertions'
          TabOrder = 3
        end
      end
    end
    object tabFiles: TTabSheet
      Caption = 'Files'
      ImageIndex = 1
      object reFiles: TRichEdit
        Left = 8
        Top = 16
        Width = 381
        Height = 289
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
        OnChange = reFilesChange
      end
      object btnAdd: TButton
        Left = 24
        Top = 312
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '&Add...'
        TabOrder = 1
        OnClick = btnAddClick
      end
    end
  end
  object btnOK: TButton
    Left = 248
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 1
    OnClick = btnOKClick
  end
  object btnCancel: TButton
    Left = 328
    Top = 416
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'dpk'
    Filter = 'Package files|*.dpk;bpk|All files|*.*'
    InitialDir = '.'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofNoReadOnlyReturn, ofEnableSizing]
    Title = 'Select packages to modify'
    Left = 276
    Top = 304
  end
end
