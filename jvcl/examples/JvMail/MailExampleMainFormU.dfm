object MailExampleMainForm: TMailExampleMainForm
  Left = 316
  Top = 201
  Width = 565
  Height = 456
  Caption = 'JvMail example'
  Color = clBtnFace
  Constraints.MinHeight = 350
  Constraints.MinWidth = 400
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
  object ClientLabel: TLabel
    Left = 464
    Top = 8
    Width = 73
    Height = 33
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'ClientLabel'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 16
    Height = 13
    Caption = '&To:'
    FocusControl = ToEdit
  end
  object Label2: TLabel
    Left = 8
    Top = 36
    Width = 36
    Height = 13
    Caption = '&Subject'
    FocusControl = SubjectEdit
  end
  object Label3: TLabel
    Left = 8
    Top = 136
    Width = 24
    Height = 13
    Caption = '&Body'
    FocusControl = BodyEdit
  end
  object Label4: TLabel
    Left = 8
    Top = 56
    Width = 59
    Height = 13
    Caption = 'Attachments'
  end
  object Label5: TLabel
    Left = 240
    Top = 11
    Width = 17
    Height = 13
    Caption = '&CC:'
    FocusControl = CcEdit
  end
  object ClientsListView: TListView
    Left = 104
    Top = 335
    Width = 450
    Height = 89
    Anchors = [akLeft, akRight, akBottom]
    Columns = <
      item
        Caption = 'KeyValue'
        Width = 80
      end
      item
        Caption = 'Client'
        Width = 80
      end
      item
        Caption = 'Path'
        Width = 240
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 9
    ViewStyle = vsReport
    OnCustomDrawItem = ClientsListViewCustomDrawItem
    OnSelectItem = ClientsListViewSelectItem
  end
  object DownloadsListView: TListView
    Left = 104
    Top = 335
    Width = 450
    Height = 89
    Anchors = [akLeft, akRight, akBottom]
    Columns = <
      item
        Caption = 'Subject'
        Width = 300
      end
      item
        Caption = 'From'
        Width = 80
      end
      item
        Caption = 'Date'
        Width = 240
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 10
    ViewStyle = vsReport
  end
  object ClientTypeGroupBox: TGroupBox
    Left = 8
    Top = 331
    Width = 81
    Height = 93
    Anchors = [akLeft, akBottom]
    Caption = '&Client type'
    TabOrder = 8
    object AutomaticRadioBtn: TRadioButton
      Left = 8
      Top = 16
      Width = 70
      Height = 17
      Caption = '&Automatic'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = AutomaticRadioBtnClick
    end
    object MapiRadioBtn: TRadioButton
      Left = 8
      Top = 40
      Width = 70
      Height = 17
      Caption = '&MAPI'
      TabOrder = 1
      OnClick = AutomaticRadioBtnClick
    end
    object DirectRadioBtn: TRadioButton
      Left = 8
      Top = 64
      Width = 70
      Height = 17
      Caption = '&Direct'
      TabOrder = 2
      OnClick = AutomaticRadioBtnClick
    end
  end
  object ToEdit: TEdit
    Left = 48
    Top = 8
    Width = 185
    Height = 21
    TabOrder = 0
  end
  object SubjectEdit: TEdit
    Left = 48
    Top = 32
    Width = 401
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object BodyEdit: TRichEdit
    Left = 8
    Top = 152
    Width = 545
    Height = 169
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideScrollBars = False
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object SendBtn: TButton
    Left = 464
    Top = 80
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Send'
    TabOrder = 6
    OnClick = SendBtnClick
  end
  object AttachmentMemo: TMemo
    Left = 8
    Top = 72
    Width = 441
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 3
    WordWrap = False
  end
  object AttachBtn: TButton
    Left = 464
    Top = 112
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Attach'
    TabOrder = 7
    OnClick = AttachBtnClick
  end
  object CcEdit: TEdit
    Left = 264
    Top = 8
    Width = 185
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
  end
  object DownloadBtn: TButton
    Left = 464
    Top = 48
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Download'
    TabOrder = 5
    OnClick = DownloadBtnClick
  end
  object JvMail1: TJvMail
    BlindCopy = <>
    CarbonCopy = <>
    Recipient = <>
    Left = 224
    Top = 372
  end
  object JvOpenDialog1: TJvOpenDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    ActiveControl = acListView
    ActiveStyle = asReport
    DefBtnCaption = 'Add'
    Height = 264
    Width = 426
    Left = 156
    Top = 372
  end
end
