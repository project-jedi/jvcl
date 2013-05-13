object FormCompileMessages: TFormCompileMessages
  Left = 185
  Top = 216
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Compiler Messages'
  ClientHeight = 118
  ClientWidth = 743
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox: TListBox
    Left = 0
    Top = 17
    Width = 743
    Height = 101
    Style = lbOwnerDrawFixed
    Align = alClient
    ParentShowHint = False
    PopupMenu = PopupMenu
    ShowHint = True
    TabOrder = 0
    OnDblClick = ListBoxDblClick
    OnDrawItem = ListBoxDrawItem
    OnMouseDown = ListBoxMouseDown
  end
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 743
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 742
    object LabelHelp: TLabel
      Left = 3
      Top = 2
      Width = 734
      Height = 13
      Caption = 
        'Many error messages appear because the JCL version and the JVCL ' +
        'version are not compatible. Please keep in mind that you need th' +
        'e correct JCL version.'
    end
  end
  object PopupMenu: TPopupMenu
    Left = 176
    Top = 48
    object Open1: TMenuItem
      Caption = '&Open'
      OnClick = ListBoxDblClick
    end
    object MenuNotepad: TMenuItem
      Caption = 'Open in &Notepad'
      OnClick = ListBoxDblClick
    end
  end
end
