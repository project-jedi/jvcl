object frmMessageDlgEditor: TfrmMessageDlgEditor
  Left = 406
  Top = 185
  Width = 518
  Height = 540
  Caption = 'MessageDlg editor'
  Color = clBtnFace
  Constraints.MaxWidth = 518
  Constraints.MinWidth = 518
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblSource: TLabel
    Left = 15
    Top = 405
    Width = 37
    Height = 13
    Caption = 'Source:'
  end
  object btnClose: TButton
    Left = 430
    Top = 485
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    TabOrder = 0
    OnClick = btnCloseClick
  end
  object gbDlgType: TGroupBox
    Left = 5
    Top = 0
    Width = 500
    Height = 121
    Caption = ' Dialog type '
    TabOrder = 1
    object imgWarning: TImage
      Left = 10
      Top = 15
      Width = 32
      Height = 32
      OnClick = imgWarningClick
    end
    object imgError: TImage
      Left = 10
      Top = 50
      Width = 32
      Height = 32
      OnClick = imgErrorClick
    end
    object imgInformation: TImage
      Left = 10
      Top = 85
      Width = 32
      Height = 32
      OnClick = imgInformationClick
    end
    object imgConfirmation: TImage
      Left = 175
      Top = 15
      Width = 32
      Height = 32
      OnClick = imgConfirmationClick
    end
    object imgCustom: TImage
      Left = 175
      Top = 50
      Width = 32
      Height = 32
      Stretch = True
      OnClick = imgCustomClick
    end
    object rbWarning: TRadioButton
      Left = 60
      Top = 20
      Width = 108
      Height = 13
      Caption = 'mtWarning'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbWarningClick
    end
    object rbError: TRadioButton
      Left = 60
      Top = 60
      Width = 108
      Height = 13
      Caption = 'mtError'
      TabOrder = 1
      OnClick = rbErrorClick
    end
    object rbInformation: TRadioButton
      Left = 60
      Top = 95
      Width = 101
      Height = 13
      Caption = 'mtInformation'
      TabOrder = 2
      OnClick = rbInformationClick
    end
    object rbConfirmation: TRadioButton
      Left = 225
      Top = 20
      Width = 108
      Height = 13
      Caption = 'mtConfirmation'
      TabOrder = 3
      OnClick = rbConfirmationClick
    end
    object rbCustom: TRadioButton
      Left = 225
      Top = 60
      Width = 108
      Height = 13
      Caption = 'mtCustom'
      TabOrder = 4
      OnClick = rbCustomClick
    end
    object btnSelectIcon: TButton
      Left = 340
      Top = 55
      Width = 75
      Height = 25
      Caption = 'Select icon'
      Enabled = False
      TabOrder = 5
      OnClick = btnSelectIconClick
    end
    object edCustomTitle: TEdit
      Left = 385
      Top = 90
      Width = 101
      Height = 21
      TabOrder = 6
      Text = 'Dialog title'
      OnChange = edCustomTitleChange
    end
    object cxCustomTitle: TCheckBox
      Left = 225
      Top = 95
      Width = 81
      Height = 13
      Caption = 'Custom title:'
      TabOrder = 7
      OnClick = cxCustomTitleClick
    end
  end
  object gbButtons: TGroupBox
    Left = 5
    Top = 125
    Width = 500
    Height = 116
    Caption = ' Buttons '
    TabOrder = 2
    object lblDefaultButton: TLabel
      Left = 10
      Top = 90
      Width = 37
      Height = 13
      Caption = 'Default:'
    end
    object lblCancelButton: TLabel
      Left = 175
      Top = 90
      Width = 36
      Height = 13
      Caption = 'Cancel:'
    end
    object lblHelpButton: TLabel
      Left = 340
      Top = 90
      Width = 25
      Height = 13
      Caption = 'Help:'
    end
    object rbStdButtons: TRadioButton
      Left = 10
      Top = 20
      Width = 113
      Height = 17
      Caption = 'Standard buttons'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbStdButtonsClick
    end
    object rbCustomButtons: TRadioButton
      Left = 175
      Top = 20
      Width = 316
      Height = 17
      Caption = 'Custom buttons (Specify <button caption>=<Result code>)'
      TabOrder = 1
      OnClick = rbCustomButtonsClick
    end
    object clbStdButtons: TCheckListBox
      Left = 10
      Top = 36
      Width = 151
      Height = 43
      OnClickCheck = clbStdButtonsClickCheck
      IntegralHeight = True
      ItemHeight = 13
      Items.Strings = (
        'mbYes'
        'mbNo'
        'mbOK'
        'mbCancel'
        'mbAbort'
        'mbRetry'
        'mbIgnore'
        'mbAll'
        'mbNoToAll'
        'mbYesToAll'
        'mbHelp')
      TabOrder = 2
    end
    object mmCustomButtons: TMemo
      Left = 175
      Top = 35
      Width = 316
      Height = 45
      Enabled = False
      Lines.Strings = (
        'Yes=mrYes'
        'No=mrNo'
        'OK=mrOK'
        'Cancel=mrCancel'
        'Abort=mrAbort'
        'Retry=mrRetry'
        'Ignore=mrIgnore'
        'All=mrAll'
        'NoToAll=mrNoToAll'
        'YesToAll=mrYesToAll'
        'Help=mrHelp')
      ScrollBars = ssVertical
      TabOrder = 3
      OnChange = mmCustomButtonsChange
    end
    object cbDefaultButton: TComboBox
      Left = 60
      Top = 85
      Width = 101
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      OnChange = cbDefaultButtonChange
    end
    object cbCancelButton: TComboBox
      Left = 225
      Top = 85
      Width = 101
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      OnChange = cbCancelButtonChange
    end
    object cbHelpButton: TComboBox
      Left = 390
      Top = 85
      Width = 101
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 6
      OnChange = cbHelpButtonChange
    end
  end
  object gbOther: TGroupBox
    Left = 5
    Top = 245
    Width = 500
    Height = 151
    Caption = ' Other settings '
    TabOrder = 3
    object lblHelpContext: TLabel
      Left = 10
      Top = 45
      Width = 37
      Height = 13
      Caption = 'HlpCtx::'
    end
    object lblMessage: TLabel
      Left = 10
      Top = 73
      Width = 46
      Height = 13
      Caption = 'Message:'
    end
    object lblAutoCloseUnit: TLabel
      Left = 120
      Top = 120
      Width = 46
      Height = 13
      Caption = 'second(s)'
    end
    object rbCenterScreen: TRadioButton
      Left = 10
      Top = 20
      Width = 113
      Height = 13
      Caption = 'Screen center'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = rbCenterScreenClick
    end
    object rbMainFormCenter: TRadioButton
      Left = 340
      Top = 20
      Width = 113
      Height = 13
      Caption = 'Main form center'
      TabOrder = 1
      OnClick = rbMainFormCenterClick
    end
    object rbActiveFormCenter: TRadioButton
      Left = 175
      Top = 20
      Width = 113
      Height = 13
      Caption = 'Active form center'
      TabOrder = 2
      OnClick = rbActiveFormCenterClick
    end
    object edHelpCtx: TEdit
      Left = 60
      Top = 40
      Width = 101
      Height = 21
      TabOrder = 3
      Text = '0'
      OnChange = edHelpCtxChange
    end
    object cxIsDSADialog: TCheckBox
      Left = 175
      Top = 45
      Width = 46
      Height = 13
      Caption = 'DSA:'
      TabOrder = 4
      OnClick = cxIsDSADialogClick
    end
    object edDSA_ID: TEdit
      Left = 225
      Top = 40
      Width = 101
      Height = 21
      Enabled = False
      TabOrder = 5
      Text = '0'
      OnChange = edDSA_IDChange
    end
    object mmMessage: TMemo
      Left = 60
      Top = 71
      Width = 431
      Height = 35
      ScrollBars = ssVertical
      TabOrder = 6
      OnChange = mmMessageChange
    end
    object cxAutoClose: TCheckBox
      Left = 10
      Top = 118
      Width = 76
      Height = 17
      Caption = 'Auto close:'
      TabOrder = 7
      OnClick = cxAutoCloseClick
    end
    object edAutoCloseDelay: TEdit
      Left = 85
      Top = 115
      Width = 31
      Height = 21
      TabOrder = 8
      Text = '10'
      OnChange = edAutoCloseDelayChange
    end
    object cxAutoCloseShow: TCheckBox
      Left = 175
      Top = 120
      Width = 106
      Height = 17
      Caption = 'Show countdown'
      TabOrder = 9
      OnClick = cxAutoCloseShowClick
    end
  end
  object mmSource: TMemo
    Left = 65
    Top = 405
    Width = 431
    Height = 69
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 4
    WordWrap = False
  end
  object btnTest: TButton
    Left = 350
    Top = 485
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 5
    OnClick = btnTestClick
  end
end
