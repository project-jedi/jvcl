object CustomizeHelpDlg: TCustomizeHelpDlg
  Left = 230
  Top = 127
  BorderStyle = bsDialog
  Caption = 'User Help Files'
  ClientHeight = 194
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poDefaultPosOnly
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 4
    Width = 87
    Height = 13
    Caption = '&Help menu items:  '
    FocusControl = HelpList
  end
  object Label2: TLabel
    Left = 8
    Top = 115
    Width = 123
    Height = 13
    Caption = '&Assosiated help file path:  '
    FocusControl = FileName
  end
  object FileName: TJvFilenameEdit
    Left = 8
    Top = 131
    Width = 321
    Height = 21
    AddQuotes = False
    DefaultExt = 'HLP'
    Filter = 'Help files (*.hlp)|*.hlp|All files (*.*)|*.*'
    DialogOptions = [ofHideReadOnly, ofFileMustExist]
    DialogTitle = 'Browse Help Files'
    ButtonFlat = False
    TabOrder = 1
    OnChange = FileNameChange
  end
  object HelpList: TJvDrawGrid
    Left = 8
    Top = 20
    Width = 321
    Height = 91
    ColCount = 2
    DefaultColWidth = 23
    DefaultRowHeight = 17
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goDrawFocusSelected, goRowMoving, goEditing]
    ScrollBars = ssVertical
    TabOrder = 0
    OnDrawCell = HelpListDrawCell
    OnGetEditText = HelpListGetEditText
    OnKeyDown = HelpListKeyDown
    OnRowMoved = HelpListRowMoved
    OnSelectCell = HelpListSelectCell
    OnSetEditText = HelpListSetEditText
    DrawButtons = False
    OnGetEditLimit = HelpListGetEditLimit
    ColWidths = (
      23
      428)
  end
  object OkBtn: TButton
    Left = 169
    Top = 163
    Width = 77
    Height = 25
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 253
    Top = 163
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object FormStorage: TJvFormStorage
    StoredProps.Strings = (
      'FileName.InitialDir')
    StoredValues = <>
    Left = 8
    Top = 159
  end
end
