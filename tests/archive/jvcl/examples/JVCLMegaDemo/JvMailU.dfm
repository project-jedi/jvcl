object JvMailForm: TJvMailForm
  Left = 225
  Top = 150
  Width = 708
  Height = 500
  Caption = 'JvMailForm'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    700
    475)
  PixelsPerInch = 96
  TextHeight = 13
  object ClientLabel: TLabel
    Left = 548
    Top = 24
    Width = 73
    Height = 33
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'ClientLabel'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 80
    Top = 28
    Width = 16
    Height = 13
    Caption = '&To:'
    FocusControl = ToEdit
  end
  object Label2: TLabel
    Left = 80
    Top = 52
    Width = 36
    Height = 13
    Caption = '&Subject'
    FocusControl = SubjectEdit
  end
  object Label3: TLabel
    Left = 80
    Top = 152
    Width = 24
    Height = 13
    Caption = '&Body'
    FocusControl = BodyEdit
  end
  object Label4: TLabel
    Left = 80
    Top = 72
    Width = 59
    Height = 13
    Caption = 'Attachments'
  end
  object Label5: TLabel
    Left = 312
    Top = 27
    Width = 17
    Height = 13
    Caption = '&CC:'
    FocusControl = CcEdit
  end
  object ClientTypeGroupBox: TGroupBox
    Left = 80
    Top = 367
    Width = 81
    Height = 93
    Anchors = [akLeft, akBottom]
    Caption = '&Client type'
    TabOrder = 0
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
  object ClientsListView: TListView
    Left = 176
    Top = 371
    Width = 462
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
    TabOrder = 1
    ViewStyle = vsReport
    OnCustomDrawItem = ClientsListViewCustomDrawItem
    OnSelectItem = ClientsListViewSelectItem
  end
  object ToEdit: TEdit
    Left = 120
    Top = 24
    Width = 185
    Height = 21
    TabOrder = 2
  end
  object SubjectEdit: TEdit
    Left = 120
    Top = 48
    Width = 413
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object BodyEdit: TRichEdit
    Left = 80
    Top = 168
    Width = 557
    Height = 189
    Anchors = [akLeft, akTop, akRight, akBottom]
    HideScrollBars = False
    PlainText = True
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object SendBtn: TButton
    Left = 548
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Send'
    TabOrder = 5
    OnClick = SendBtnClick
  end
  object AttachmentMemo: TMemo
    Left = 80
    Top = 88
    Width = 453
    Height = 65
    Anchors = [akLeft, akTop, akRight]
    ScrollBars = ssBoth
    TabOrder = 6
    WordWrap = False
  end
  object AttachBtn: TButton
    Left = 548
    Top = 128
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Attach'
    TabOrder = 7
    OnClick = AttachBtnClick
  end
  object CcEdit: TEdit
    Left = 336
    Top = 24
    Width = 197
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 8
  end
  object JvMail1: TJvMail
    BlindCopy = <>
    CarbonCopy = <>
    Recipient = <>
    Left = 264
    Top = 388
  end
  object JvOpenDialog1: TJvOpenDialog
    Filter = 'All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofAllowMultiSelect, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    ActiveControl = acListView
    ActiveStyle = asReport
    DefBtnCaption = 'Add'
    Height = 264
    Width = 426
    Left = 196
    Top = 388
  end
end
