object frmOptions: TfrmOptions
  Left = 402
  Top = 138
  Width = 425
  Height = 445
  Caption = 'Options'
  Color = clBtnFace
  Constraints.MinHeight = 445
  Constraints.MinWidth = 425
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object pcOptions: TPageControl
    Left = 6
    Top = 6
    Width = 407
    Height = 356
    ActivePage = tabGeneral
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tabGeneral: TTabSheet
      Caption = 'General'
      object Label1: TLabel
        Left = 12
        Top = 12
        Width = 78
        Height = 13
        Caption = '&Question image:'
        FocusControl = edQImage
      end
      object Label2: TLabel
        Left = 12
        Top = 64
        Width = 71
        Height = 13
        Caption = '&Answer image:'
        FocusControl = edAImage
      end
      object Label3: TLabel
        Left = 12
        Top = 168
        Width = 55
        Height = 13
        Caption = '&Stylesheet:'
        FocusControl = edStyleSheet
      end
      object Label4: TLabel
        Left = 12
        Top = 116
        Width = 124
        Height = 13
        Caption = 'I&mage path (in HTML file):'
        FocusControl = edImagePath
      end
      object Label5: TLabel
        Left = 12
        Top = 221
        Width = 128
        Height = 13
        Caption = 'S&tylesheet path (in HTML):'
        FocusControl = edStyleSheetPath
      end
      object Label6: TLabel
        Left = 12
        Top = 270
        Width = 73
        Height = 13
        Caption = '&Document title:'
        FocusControl = edTitle
      end
      object edQImage: TJvDotNetFilenameEdit
        Left = 12
        Top = 30
        Width = 371
        Height = 21
        OnAfterDialog = StripPathFromFilename
        AddQuotes = False
        Filter = 
          'Image files|*.gif;*.jpg;*.jpeg;*.png|GIF files|*.gif|JPEG files|' +
          '*.jpg;*.jpeg|PNG files|*.png|All files|*.*'
        DialogTitle = 'Select image'
        ButtonFlat = True
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'q.gif'
      end
      object edAImage: TJvDotNetFilenameEdit
        Left = 12
        Top = 82
        Width = 371
        Height = 21
        OnAfterDialog = StripPathFromFilename
        AddQuotes = False
        Filter = 
          'Image files|*.gif;*.jpg;*.jpeg;*.png|GIF files|*.gif|JPEG files|' +
          '*.jpg;*.jpeg|PNG files|*.png|All files|*.*'
        DialogTitle = 'Select image'
        ButtonFlat = True
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'a.gif'
      end
      object edStyleSheet: TJvDotNetFilenameEdit
        Left = 12
        Top = 186
        Width = 371
        Height = 21
        OnAfterDialog = StripPathFromFilename
        AddQuotes = False
        Filter = 'Style sheets|*.css|All files|*.*'
        DialogTitle = 'Select stylesheet'
        ButtonFlat = True
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        Text = 'default.css'
      end
      object edImagePath: TJvDotNetEdit
        Left = 12
        Top = 134
        Width = 371
        Height = 21
        PasswordChar = #0
        GroupIndex = -1
        MaxPixel.Font.Charset = DEFAULT_CHARSET
        MaxPixel.Font.Color = clWindowText
        MaxPixel.Font.Height = -11
        MaxPixel.Font.Name = 'MS Sans Serif'
        MaxPixel.Font.Style = []
        Modified = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 2
        Text = 'images/'
      end
      object edStyleSheetPath: TJvDotNetEdit
        Left = 12
        Top = 239
        Width = 371
        Height = 21
        PasswordChar = #0
        GroupIndex = -1
        MaxPixel.Font.Charset = DEFAULT_CHARSET
        MaxPixel.Font.Color = clWindowText
        MaxPixel.Font.Height = -11
        MaxPixel.Font.Name = 'MS Sans Serif'
        MaxPixel.Font.Style = []
        Modified = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 4
        Text = 'styles/'
      end
      object edTitle: TJvDotNetEdit
        Left = 12
        Top = 288
        Width = 371
        Height = 21
        PasswordChar = #0
        GroupIndex = -1
        MaxPixel.Font.Charset = DEFAULT_CHARSET
        MaxPixel.Font.Color = clWindowText
        MaxPixel.Font.Height = -11
        MaxPixel.Font.Name = 'MS Sans Serif'
        MaxPixel.Font.Style = []
        Modified = False
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 5
        Text = 'Frequently Asked Questions'
      end
    end
    object tabTemplates: TTabSheet
      BorderWidth = 2
      Caption = 'Templates'
      ImageIndex = 2
      object roFooter: TJvRollOut
        Left = 0
        Top = 300
        Width = 395
        Height = 22
        Align = alTop
        BorderWidth = 4
        Caption = '&Footer'
        Collapsed = True
        GroupIndex = 1
        ShowFocus = False
        TabOrder = 2
        ToggleAnywhere = False
        OnExpand = roHeaderExpand
        FAWidth = 145
        FAHeight = 278
        FCWidth = 22
        FCHeight = 22
        object reFooter: TJvDotNetRichEdit
          Left = 5
          Top = 25
          Width = 385
          Height = 248
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PlainText = True
          TabOrder = 0
          WordWrap = False
          Zoom = 100
        end
      end
      object roHeader: TJvRollOut
        Left = 0
        Top = 0
        Width = 395
        Height = 278
        Align = alTop
        BorderWidth = 4
        Caption = '&Header'
        GroupIndex = 1
        ShowFocus = False
        TabOrder = 0
        ToggleAnywhere = False
        OnExpand = roHeaderExpand
        FAWidth = 145
        FAHeight = 278
        FCWidth = 22
        FCHeight = 22
        object reHeader: TJvDotNetRichEdit
          Left = 5
          Top = 25
          Width = 385
          Height = 248
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PlainText = True
          TabOrder = 0
          WordWrap = False
          Zoom = 100
        end
      end
      object roItem: TJvRollOut
        Left = 0
        Top = 278
        Width = 395
        Height = 22
        Align = alTop
        BorderWidth = 4
        Caption = '&Item'
        Collapsed = True
        GroupIndex = 1
        ShowFocus = False
        TabOrder = 1
        ToggleAnywhere = False
        OnExpand = roHeaderExpand
        FAWidth = 145
        FAHeight = 278
        FCWidth = 22
        FCHeight = 22
        object reItem: TJvDotNetRichEdit
          Left = 5
          Top = 25
          Width = 385
          Height = 248
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          PlainText = True
          TabOrder = 0
          WordWrap = False
          Zoom = 100
        end
      end
    end
  end
  object btnOK: TJvDotNetButton
    Left = 242
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object btnCancel: TJvDotNetButton
    Left = 322
    Top = 380
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
end
