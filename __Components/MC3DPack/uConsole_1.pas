unit uConsole_1;

interface

uses SysUtils, Classes, Controls, ExtCtrls, Forms, Buttons, StdCtrls, Graphics,
  Windows, Dialogs, ComCtrls, Messages, Menus, ShellAPI, DB,
  uCript_1, uService_1,
  SynMemo, SynEdit, SynEditHighlighter, SynEditSearch, SynEditTypes,
  SynHighlighterPAS,
  IFPS3CompExec, ifpscomp,
  ifpii_std, ifpii_stdctrls, ifpii_sysutils, ifpii_forms, ifpii_graphics,
  ifpii_controls, ifpii_classes, ifpii_word, ifpii_dialogs,
  ifpii_excel, ifpii_comctrls, ifpii_extctrls,
  ifpiir_std, ifpiir_stdctrls, ifpiir_forms, ifpiir_graphics, ifpiir_controls,
  ifpiir_classes, ifpir_word, ifpiir_dialogs, ifpir_excel,
  ifpiir_comctrls, ifpiir_extctrls;

type
  TCCOptionType = (cotMemoColor, cotActiveLineColor);

  TCCOptionChange = procedure (Sender: TObject; OptionType: TCCOptionType) of object;

  TConsoleColorOptions = class(TPersistent)
  private
    FOnOptionChange: TCCOptionChange;
    FMemoColor: TColor;
    FActiveLineColor: TColor;
    procedure SetActiveLineColor(const Value: TColor);
    procedure SetMemoColor(const Value: TColor);
  protected
    property OnOptionChange: TCCOptionChange read FOnOptionChange write FOnOptionChange;
  public
    constructor Create;
  published
    property MemoColor: TColor read FMemoColor write SetMemoColor;
    property ActiveLineColor: TColor read FActiveLineColor write SetActiveLineColor;
  end;

  TActionType = (atSearch, atReplace, atSearchNext, atSearchPrev);

  TFSearchForm = class(TForm)
    PageSearchReplace: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label3: TLabel;
    EditSearch: TEdit;
    gbSearchOptions: TGroupBox;
    cbSearchCaseSensitive: TCheckBox;
    cbSearchWholeWords: TCheckBox;
    cbSearchFromCursor: TCheckBox;
    cbSearchSelectedOnly: TCheckBox;
    rgSearchDirection: TRadioGroup;
    btnOk: TButton;
    btnCancel: TButton;
    EditReplace: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure PageSearchReplaceChange(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormShow(Sender: TObject);
  private
    procedure SetActionType(const Value: TActionType);
    function GetActionType: TActionType;
    function GetBackward: Boolean;
    function GetCaseSensitive: Boolean;
    function GetFromCursor: Boolean;
    function GetSelectedOnly: Boolean;
    function GetWholeWords: Boolean;
    procedure SetBackward(const Value: Boolean);
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetFromCursor(const Value: Boolean);
    procedure SetSelectedOnly(const Value: Boolean);
    procedure SetWholeWords(const Value: Boolean);
  public
    property Backward: Boolean read GetBackward write SetBackward;
    property CaseSensitive: Boolean read GetCaseSensitive write SetCaseSensitive;
    property WholeWords: Boolean read GetWholeWords write SetWholeWords;
    property FromCursor: Boolean read GetFromCursor write SetFromCursor;
    property SelectedOnly: Boolean read GetSelectedOnly write SetSelectedOnly;
    property ActionType: TActionType read GetActionType write SetActionType;
  end;

  TFConfirmDialog = class(TForm)
    lblConfirmation: TLabel;
    Image1: TImage;
    btnReplace: TButton;
    btnSkip: TButton;
    btnCancel: TButton;
    btnReplaceAll: TButton;
    procedure FormCreate(Sender: TObject);
  public
    procedure PrepareShow(AEditorRect: TRect; X, Y1, Y2: integer; AReplaceText: string);
  end;

  TConsoleButton = (cbRun, cbSave, cbNew, cbDelete, cbFind, cbReplace);

  TConsoleColorTheme = (cctLight, cctDark);

  TCustomConsole = class (TCustomPanel)
  private
    // Variables
    FChanged: Boolean;
    FLogHeight: Integer;
    // Visual
    FLogHide: TSpeedButton;
    FSave: TSpeedButton;
    FAdd: TSpeedButton;
    FDel: TSpeedButton;
    FRun: TSpeedButton;
    FFind: TSpeedButton;
    FReplace: TSpeedButton;
    FCriptCheck: TCheckBox;
    FSynMemo: TSynMemo;
    FHeadPanel: TPanel;
    FComboBox: TComboBox;
    FResultMemo: TMemo;
    FResultSplitter: TSPlitter;
    //
    FDownTempPanel: TPanel;
    // Dialogs
    FSearchForm: TFSearchForm;
    FConfirmDialog: TFConfirmDialog;
    // Unvisual
    FHighlighter: TSynCustomHighlighter;
    FSearch: TSynEditSearch;
    FThemePopup: TPopupMenu;
    // Properties
    FSourcePath: String;
    FFilesExt: String;
    FConsoleColorTheme: TConsoleColorTheme;
    FAuthor: string;
    procedure SetHighlighter(const Value: TSynCustomHighlighter);
    procedure SetConsoleColorTheme(const Value: TConsoleColorTheme);
    procedure SetSourcePath(const Value: String);
    procedure SetFilesExt(const Value: String);
    function GetEncriptQueries: Boolean;
    procedure SetEncriptQueries(const Value: Boolean);
    // Event handlers
    procedure ButtonClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure ComboDropDown(Sender: TObject);
    procedure SynMemoChange(Sender: TObject);
    procedure HideLogBtnClick(Sender: TObject);
    procedure CriptCheckClick(Sender: TObject);
    procedure SynKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SynEditorReplaceText(Sender: TObject; const ASearch, AReplace: String;
      Line, Column: Integer; var Action: TSynReplaceAction);
    procedure PopupClick(Sender: TObject);
    // Some functions
    procedure EnableButtons;
    function GetFileList: TStrings;
  protected
    procedure DoSearchReplace(Search, Replace: String; Action: TActionType);
    procedure ChangeColorTheme(AConsoleColorTheme: TConsoleColorTheme); virtual;
    procedure Run; virtual;
    property SynHighLighter: TSynCustomHighlighter read FHighlighter write SetHighlighter;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SearchReplace(Search, Replace: String; Action: TActionType);
  published
    property Align default alClient;
    property Anchors;
    property Height;
    property TabOrder;
    property Visible;
    property Width;
    property Author: string read FAuthor write FAuthor;
    property ConsoleColorTheme: TConsoleColorTheme read FConsoleColorTheme
      write SetConsoleColorTheme default cctLight;
    property EncriptScripts: Boolean read GetEncriptQueries
      write SetEncriptQueries default False;
    property FilesExtention: String read FFilesExt write SetFilesExt;
    property SourcePath: String read FSourcePath write SetSourcePath;
  end;

  TAfterCompImportEvent = procedure (Sender: TObject; x: TIFPSCompileTimeClassesImporter) of object;
  TAfterExecImportEvent = procedure (Sender: TObject; x: TIFPSRuntimeClassImporter) of object;

  TScriptCompiler = class (TComponent)
  private
    FCompExec: TIFPS3CompExec;
    FDllPlugin: TIFPS3DllPlugin;
    FClassesPlugin: TIFPS3ClassesPlugin;
    FScript: TStrings;
    FExportPath: String;
    FOutParams: TStrings;
    FAfterExecImport: TAfterExecImportEvent;
    FAfterCompImport: TAfterCompImportEvent;
    procedure SetExportPath(const Value: String);
    procedure SetScript(const Value: TStrings);
    procedure SetCompExec(const Value: TIFPS3CompExec);
    procedure Compile(Sender: TIFPS3CompExec);
    procedure Execute(Sender: TIFPS3CompExec);
    procedure CompImport(Sender: TObject; x: TIFPSCompileTimeClassesImporter);
    procedure ExecImport(Sender: TObject; x: TIFPSRuntimeClassImporter);
    procedure DoAferCompImport(x: TIFPSCompileTimeClassesImporter);
    procedure DoAferExecImport(x: TIFPSRuntimeClassImporter);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Run(Script: TStrings); reintroduce; overload;
    procedure Run(Script: String); reintroduce; overload;
    procedure Run; reintroduce; overload;
    procedure RunFromFile(FileName: String);
    procedure LoadFromFile(FileName: String);
    procedure ReplaceVar(OldPattern, NewPattern: string);
    property CompExec: TIFPS3CompExec read FCompExec write SetCompExec;
    property OutParams: TStrings read FOutParams write FOutParams;
  published
    property Script: TStrings read FScript write SetScript;
    property ExportPath: String read FExportPath write SetExportPath;
    property AfterCompImport: TAfterCompImportEvent read FAfterCompImport write FAfterCompImport;
    property AfterExecImport: TAfterExecImportEvent read FAfterExecImport write FAfterExecImport;
  end;

  TScriptConsole = class (TCustomConsole)
  private
    FScriptCompiler: TScriptCompiler;
    procedure SetScriptCompiler(const Value: TScriptCompiler);
  protected
    procedure ChangeColorTheme(AConsoleColorTheme: TConsoleColorTheme); override;
    procedure Run; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ScriptCompiler: TScriptCompiler read FScriptCompiler write SetScriptCompiler;
  end;

procedure Register;

resourcestring
  SAskReplaceText = 'Заменить это вхождение на "%s"?';

implementation

{$R 'Plug.dcr'}

uses
  Masks;

procedure Register;
begin
  RegisterComponents('MC3D', [TScriptConsole, TScriptCompiler]);
end;

const
  BUTTON_WIDTH = 26;

{ --------------------------- TConsoleColorOptions --------------------------- }

constructor TConsoleColorOptions.Create;
begin
  FMemoColor := clWhite;
  FActiveLineColor := RGB(250, 255, 230);
end;

procedure TConsoleColorOptions.SetActiveLineColor(const Value: TColor);
begin
  FActiveLineColor := Value;
  if Assigned(FOnOptionChange) then
    FOnOptionChange(Self, cotActiveLineColor);
end;

procedure TConsoleColorOptions.SetMemoColor(const Value: TColor);
begin
  FMemoColor := Value;
  if Assigned(FOnOptionChange) then
    FOnOptionChange(Self, cotMemoColor);
end;

{ ------------------------------- TFSearchForm ------------------------------- }

{$R uFindForm.dfm}

procedure TFSearchForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFSearchForm.btnOkClick(Sender: TObject);
begin
  Close;
  with Owner as TCustomConsole do
    DoSearchReplace(EditSearch.Text, EditReplace.Text, ActionType);
end;

procedure TFSearchForm.FormCreate(Sender: TObject);
begin
  PageSearchReplace.ActivePageIndex := 0;
end;

procedure TFSearchForm.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  case Msg.CharCode of
    VK_RETURN: btnOkClick(btnOk);
    VK_ESCAPE: btnCancelClick(btnCancel);
  end;
end;

procedure TFSearchForm.FormShow(Sender: TObject);
begin
  EditSearch.SetFocus;
end;

function TFSearchForm.GetActionType: TActionType;
begin
  Result := TActionType(PageSearchReplace.ActivePageIndex);
end;

function TFSearchForm.GetBackward: Boolean;
begin
  Result := rgSearchDirection.ItemIndex = 1;
end;

function TFSearchForm.GetCaseSensitive: Boolean;
begin
  Result := cbSearchCaseSensitive.Checked;
end;

function TFSearchForm.GetFromCursor: Boolean;
begin
  Result := cbSearchFromCursor.Checked;
end;

function TFSearchForm.GetSelectedOnly: Boolean;
begin
  Result := cbSearchSelectedOnly.Checked;
end;

function TFSearchForm.GetWholeWords: Boolean;
begin
  Result := cbSearchWholeWords.Checked;
end;

procedure TFSearchForm.PageSearchReplaceChange(Sender: TObject);
begin
  Caption := TPageControl(Sender).ActivePage.Caption;
  btnOk.Caption := TPageControl(Sender).ActivePage.Caption;

  EditReplace.TabOrder := 1;

  Label1.Parent := TPageControl(Sender).ActivePage;
  EditSearch.Parent := TPageControl(Sender).ActivePage;
  gbSearchOptions.Parent := TPageControl(Sender).ActivePage;
  rgSearchDirection.Parent := TPageControl(Sender).ActivePage;
  btnOk.Parent := TPageControl(Sender).ActivePage;
  btnCancel.Parent := TPageControl(Sender).ActivePage;
end;

procedure TFSearchForm.SetActionType(const Value: TActionType);
begin
  PageSearchReplace.ActivePageIndex := Integer(Value);
  PageSearchReplaceChange(PageSearchReplace);
end;

procedure TFSearchForm.SetBackward(const Value: Boolean);
begin
  case Value of
    True:  rgSearchDirection.ItemIndex := 1;
    False: rgSearchDirection.ItemIndex := 0;
  end;
end;

procedure TFSearchForm.SetCaseSensitive(const Value: Boolean);
begin
  cbSearchCaseSensitive.Checked := Value;
end;

procedure TFSearchForm.SetFromCursor(const Value: Boolean);
begin
  cbSearchFromCursor.Checked := Value;
end;

procedure TFSearchForm.SetSelectedOnly(const Value: Boolean);
begin
  cbSearchSelectedOnly.Checked := Value;
end;

procedure TFSearchForm.SetWholeWords(const Value: Boolean);
begin
  cbSearchWholeWords.Checked := Value;
end;

{ ------------------------------ TFConfirmDialog ----------------------------- }

{$R uConfirmDialog.dfm}

procedure TFConfirmDialog.FormCreate(Sender: TObject);
begin
  Image1.Picture.Icon.Handle := LoadIcon(0, IDI_QUESTION);
end;

procedure TFConfirmDialog.PrepareShow(AEditorRect: TRect; X, Y1, Y2: integer;
  AReplaceText: string);
var nW, nH: integer;
begin
  lblConfirmation.Caption := Format(SAskReplaceText, [AReplaceText]);
  nW := AEditorRect.Right - AEditorRect.Left;
  nH := AEditorRect.Bottom - AEditorRect.Top;

  if nW <= Width then
    X := AEditorRect.Left - (Width - nW) div 2
  else
    begin
      if X + Width > AEditorRect.Right then
        X := AEditorRect.Right - Width;
    end;
  if Y2 > AEditorRect.Top + MulDiv(nH, 2, 3) then
    Y2 := Y1 - Height - 4
  else Inc(Y2, 4);
  SetBounds(X, Y2, Width, Height);
end;

{ ------------------------------ TCustomConsole ------------------------------ }

const
  CONS_PASS = 'R5@AYbI2T7or2@Q4kKJ#=U62jk\4$SQQ!&%OHTY1&hcFuTBK1dM/1~$5=%6>IeX6';

constructor TCustomConsole.Create(AOwner: TComponent);
var MenuItem: TMenuItem;
begin
  inherited;
  FSourcePath := IncludeTrailingPathDelimiter(ExpandFileName('.\Source'));

  Align := alClient;
  BevelKind := bkNone;
  BevelOuter := bvNone;

  // Unvisual
  FSearch := TSynEditSearch.Create(Self);
  with FSearch do
    begin
      FreeNotification(Self);
    end;

  FThemePopup := TPopupMenu.Create(Self);
  with FThemePopup do
    begin
      MenuItem := TMenuItem.Create(Self);
      with MenuItem do
        begin
          Caption := '--- Выбор темы оформления ---';
          Enabled := False;
          FreeNotification(Self);
        end;
      Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(Self);
      with MenuItem do
        begin
          Caption := 'Светлая тема';
          OnClick := PopupClick;
          AutoCheck := True;
          RadioItem := True;
          Checked := True;
          Tag := Integer(cctLight);
          FreeNotification(Self);
        end;
      Items.Add(MenuItem);

      MenuItem := TMenuItem.Create(Self);
      with MenuItem do
        begin
          Caption := 'Темная тема';
          OnClick := PopupClick;
          AutoCheck := True;
          RadioItem := True;
          Tag := Integer(cctDark);
          FreeNotification(Self);
        end;
      Items.Add(MenuItem);

      FreeNotification(Self);
    end;

  // Dialogs
  FConfirmDialog := TFConfirmDialog.Create(Self);
  with FConfirmDialog do
    begin
      FreeNotification(Self);
    end;

  FSearchForm := TFSearchForm.Create(Self);
  with FSearchForm do
    begin
      FreeNotification(Self);
    end;

  // Visual
  FHeadPanel := TPanel.Create(Self);
  with FHeadPanel do
    begin
      Parent := Self;
      Align := AlTop;
      Height := 28;
      Caption := ' ';
      BevelKind := bkNone;
      BevelOuter := bvNone;
      FreeNotification(Self);
      PopupMenu := FThemePopup;
    end;

  FSynMemo := TSynMemo.Create(Self);
  with FSynMemo do
    begin
      SearchEngine := FSearch;
      Parent := Self;
      Height := 300;
      OnChange := SynMemoChange;
      OnReplaceText := SynEditorReplaceText;
      Constraints.MinHeight := 30;
      FreeNotification(Self);
      Highlighter := FHighlighter;
      Align := alClient;
      Color := clWhite;
      ActiveLineColor := RGB(250, 255, 230);
      with Font do
        begin
          Charset := DEFAULT_CHARSET;
          Color := clBlack;
          Size := 10;
          Name := 'Courier New';
          Style := [];
        end;
      TabOrder := 1;
      Gutter.LeftOffset := 0;
      Gutter.RightOffset := 0;
      Gutter.ShowLineNumbers := True;
      with Gutter.Font do
        begin
          Charset := DEFAULT_CHARSET;
          Color := clWindowText;
          Size := 10;
          Name := 'Courier New';
          Style := [];
        end;
      WordWrap := False;
      SelectedColor.Background := clSilver;
      SelectedColor.Foreground := clNavy;
      MaxScrollWidth := 4096;
      MaxUndo := 65536;
      Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey, eoGroupUndo,
        eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces, eoTrimTrailingSpaces];
      RightEdge := 100;
      WordWrapGlyph.Visible := False;
      WantTabs := True;
      TabWidth := 2;
      OnKeyDown := SynKeyDown;
    end;

  with TLabel.Create(Self) do
    begin
      Parent := FHeadPanel;
      Caption := 'Скрипты: ';
      AlignWithMargins := True;
      Margins.Left := 8;
      Margins.Top := 6;
      Align := alLeft;
      FreeNotification(Self);
    end;

  FComboBox := TComboBox.Create(Self);
  with FComboBox do
    begin
      Parent := FHeadPanel;
      Width := 400;
      OnChange := ComboBoxChange;
      OnDropDown := ComboDropDown;
      AlignWithMargins := True;
      Margins.Left := 4;
      Margins.Top := 4;
      FreeNotification(Self);
    end;

  FDownTempPanel := TPanel.Create(Self);
  with FDownTempPanel do
    begin
      Parent := Self;
      Height := 110;
      Caption := ' ';
      BevelKind := bkNone;
      BevelOuter := bvNone;
      Align := alBottom;
      FreeNotification(Self);
    end;

  FLogHide := TSpeedButton.Create(Self);
  with FLogHide do
    begin
      Parent := FDownTempPanel;
      Top := Self.Height;
      Height := 8;
      Flat := True;
      Align := alBottom;
      Glyph.LoadFromResourceName(hInstance, 'PANEL_SHOW_BTN');
      OnClick := HideLogBtnClick;
      FreeNotification(Self);
    end;

  FResultMemo := TMemo.Create(Self);
  with FResultMemo do
    begin
      Parent := FDownTempPanel;
      Height := 70;
      Font.Name := 'Courier New';
      Font.Color := clWhite;
      Font.Height := -13;
      Font.Style := [];
      Font.Size := 10;
      Font.Charset := DEFAULT_CHARSET;
      Align := alClient;
      Color := clBlack;
      Constraints.MinHeight := 15;
      ScrollBars := ssVertical;
      ReadOnly := True;
      FreeNotification(Self);
    end;


  FResultSplitter := TSplitter.Create(Self);
  with FResultSplitter do
    begin
      Parent := Self;
      Top := Self.Height - 1;
      Height := 3;
      ResizeStyle := rsUpdate;
      Align := alBottom;
      FreeNotification(Self);
    end;

  // Buttons
  FRun := TSpeedButton.Create(Self);
  with FRun do
    begin
      Parent := FHeadPanel;
      Width := BUTTON_WIDTH;
      Flat := True;
      AlignWithMargins := True;
      Margins.Left := 1;
      Margins.Right := 1;
      Margins.Top := 1;
      Margins.Bottom := 1;
      OnClick := ButtonClick;
      Tag := Integer(cbRun);
      ShowHint := True;
      Hint := 'Выполнить (F9)';
      NumGlyphs := 2;
      Glyph.LoadFromResourceName(hInstance, 'SQLRUN');
      FreeNotification(Self);
    end;

  FAdd := TSpeedButton.Create(Self);
  with FAdd do
    begin
      Parent := FHeadPanel;
      Width := BUTTON_WIDTH;
      Flat := True;
      AlignWithMargins := True;
      Margins.Left := 1;
      Margins.Right := 1;
      Margins.Top := 1;
      Margins.Bottom := 1;
      OnClick := ButtonClick;
      Tag := Integer(cbNew);
      ShowHint := True;
      Hint := 'Новый (Ctrl + N)';
      NumGlyphs := 2;
      Glyph.LoadFromResourceName(hInstance, 'SQLADD');
      FreeNotification(Self);
    end;

  FSave := TSpeedButton.Create(Self);
  with FSave do
    begin
      Parent := FHeadPanel;
      Width := BUTTON_WIDTH;
      Flat := True;
      AlignWithMargins := True;
      Margins.Left := 1;
      Margins.Right := 1;
      Margins.Top := 1;
      Margins.Bottom := 1;
      OnClick := ButtonClick;
      Tag := Integer(cbSave);
      ShowHint := True;
      Hint := 'Сохранить (F2)';
      NumGlyphs := 2;
      Glyph.LoadFromResourceName(hInstance, 'SQLSAV');
      FreeNotification(Self);
    end;

  FDel := TSpeedButton.Create(Self);
  with FDel do
    begin
      Parent := FHeadPanel;
      Width := BUTTON_WIDTH;
      Flat := True;
      AlignWithMargins := True;
      Margins.Left := 1;
      Margins.Right := 1;
      Margins.Top := 1;
      Margins.Bottom := 1;
      OnClick := ButtonClick;
      Tag := Integer(cbDelete);
      ShowHint := True;
      Hint := 'Удалить (F8)';
      NumGlyphs := 2;
      Glyph.LoadFromResourceName(hInstance, 'SQLDEL');
      FreeNotification(Self);
    end;

  FFind := TSpeedButton.Create(Self);
  with FFind do
    begin
      Parent := FHeadPanel;
      Width := BUTTON_WIDTH;
      Flat := True;
      AlignWithMargins := True;
      Margins.Left := 1;
      Margins.Right := 1;
      Margins.Top := 1;
      Margins.Bottom := 1;
      OnClick := ButtonClick;
      Tag := Integer(cbFind);
      ShowHint := True;
      Hint := 'Найти (Ctrl + F)'#13#10' + Найти далее (F3)'#13#10' + Найти предыдущее (Shift + F3)';
      NumGlyphs := 1;
      Glyph.LoadFromResourceName(hInstance, 'SQLSEARCH');
      FreeNotification(Self);
    end;

  FReplace := TSpeedButton.Create(Self);
  with FReplace do
    begin
      Parent := FHeadPanel;
      Width := BUTTON_WIDTH;
      Flat := True;
      AlignWithMargins := True;
      Margins.Left := 1;
      Margins.Right := 1;
      Margins.Top := 1;
      Margins.Bottom := 1;
      OnClick := ButtonClick;
      Tag := Integer(cbReplace);
      ShowHint := True;
      Hint := 'Заменить (Ctrl + R)';
      NumGlyphs := 1;
      Glyph.LoadFromResourceName(hInstance, 'SQLREPLACE');
      FreeNotification(Self);
    end;

  FCriptCheck := TCheckBox.Create(Self);
  with FCriptCheck do
    begin
      Left := 0;
      Parent := FHeadPanel;
      AlignWithMargins := True;
      Margins.Top := 8;
      Margins.Left := 2;
      Margins.Right := 0;
      Caption := 'Шифрование';
      OnClick := CriptCheckClick;
      FreeNotification(Self);
    end;

  FRun.Align := alRight;
  FDel.Align := alRight;
  FAdd.Align := alRight;
  FSave.Align := alRight;
  FFind.Align := alRight;
  FReplace.Align := alRight;
  FCriptCheck.Align := alRight;
  
  FComboBox.Align := alClient;

  ChangeColorTheme(cctLight);
  
  EnableButtons;
end;

procedure TCustomConsole.ButtonClick(Sender: TObject);
var S: String;
begin
  case TConsoleButton(TComponent(Sender).Tag) of
    cbRun: Run;
    cbSave:
      begin
        if Trim(FSynMemo.Text) = '' then exit;
        if (FComboBox.ItemIndex = -1) or (FComboBox.Text = '') then
          begin
            S := 'Скрипт ' + IntToStr(FComboBox.Items.Count + 1);
            if not InputQuery('Введите название скрипта', 'Название скрипта', S) then
              Exit;
          end
        else S := FComboBox.Items[FComboBox.ItemIndex];
        with TStringList.Create do
          begin
            Text := FSynMemo.Text;
            if S[1] = '*' then S := Copy(S, 3, Length(S));
            if FCriptCheck.Checked then
              Text := 'CR*' + StrToHexCharCodes(RC4EncString(CONS_PASS, Text));
            try
              SaveToFile(FSourcePath + S + FFilesExt);
            except
              MessageBox(Application.Handle, 'Не удалось сохранить запрос', 'Ошибка', MB_OK or MB_ICONERROR);
              Free;
              Exit;
            end;
            FComboBox.Items.Add(S);
            FComboBox.ItemIndex := FComboBox.Items.Count - 1;
            Free;
          end;
        FChanged := False;
        EnableButtons;
      end;
    cbNew:
      begin
        if Trim(FSynMemo.Text) = '' then exit;
        FSynMemo.Clear;
        FComboBox.ItemIndex := -1;
        FChanged := True;
        EnableButtons;
      end;
    cbDelete:
      if FComboBox.ItemIndex <> -1 then
        if MessageBox(Application.Handle, PAnsiChar('Вы действительно хотите удалить скрипт "' +
          FComboBox.Text + '"?'), 'Удаление', MB_YESNO or MB_ICONQUESTION) = idYes then
          begin
            FSynMemo.Clear;
            S := FComboBox.Items[FComboBox.ItemIndex];
            if S[1] = '*' then
              S := Copy(S, 2, Length(S));
            SysUtils.DeleteFile(FSourcePath + S + FFilesExt);
            FComboBox.Items.Delete(FComboBox.ItemIndex);
            FComboBox.Text := '';
            FChanged := False;
            EnableButtons;
          end;
    cbFind:
      begin
        if FSynMemo.SelAvail then
          S := FSynMemo.SelText;
        SearchReplace(S, '', atSearch);
      end;
    cbReplace:
      begin
        if FSynMemo.SelAvail then
          S := FSynMemo.SelText;
        SearchReplace(S, '', atReplace);
      end;
  end;
end;

procedure TCustomConsole.ChangeColorTheme(
  AConsoleColorTheme: TConsoleColorTheme);
begin
  case AConsoleColorTheme of
    cctLight:
      begin
        with FSynMemo do
          begin
            Color := clWhite;
            ActiveLineColor := RGB(250, 255, 230);
            Font.Color := clBlack;
            SelectedColor.Background := $00FFA448;
            SelectedColor.Foreground := clBlack;
          end;
        with FResultMemo do
          begin
            Color := clWhite;
            Font.Color := RGB(0, 36, 72);
          end;
      end;
    cctDark:
      begin
        with FSynMemo do
          begin
            Color := clNavy;
            ActiveLineColor := clNavy;
            Font.Color := clYellow;
            SelectedColor.Background := clSilver;
            SelectedColor.Foreground := clNavy;
          end;
        with FResultMemo do
          begin
            Color := clBlack;
            Font.Color := clWhite;
          end;
      end;
  end;
end;

procedure TCustomConsole.ComboBoxChange(Sender: TObject);
var
  S: String;
//  OldIndex: Integer;
begin
//  OldIndex := TComboBox(Sender).ItemIndex;
//  TComboBox(Sender).Items.Assign(GetFileList);
  S := TComboBox(Sender).Text;
  if S <> '' then
    begin
      if S[1] <> '*' then
        begin
          FSynMemo.Lines.LoadFromFile(FSourcePath + S + FFilesExt);
          FCriptCheck.Checked := False;
        end
      else
        begin
          with TStringList.Create do
            begin
              LoadFromFile(FSourcePath + Copy(S, 3, Length(S)) + FFilesExt);
              FSynMemo.Text := RC4EncString(CONS_PASS, HexCharCodesToStr(Copy(Text, 4, Length(Text) - 7)));
              Free;
            end;
          FCriptCheck.Checked := True;
        end;
    end;
  FChanged := False;
  EnableButtons;
end;

procedure TCustomConsole.ComboDropDown(Sender: TObject);
begin
  TComboBox(Sender).Items.Assign(GetFileList);
  EnableButtons;
end;

procedure TCustomConsole.CriptCheckClick(Sender: TObject);
begin
  FChanged := True;
  EnableButtons;
end;

procedure TCustomConsole.DoSearchReplace(Search, Replace: String;
  Action: TActionType);
var Options: TSynSearchOptions;
begin
  case Action of
    atReplace: Options := [ssoPrompt, ssoReplace, ssoReplaceAll];
    atSearch:  Options := [];
    atSearchNext: Options := [];
    atSearchPrev: Options := [ssoBackwards];
  end;
  
  if FSearchForm.Backward then Include(Options, ssoBackwards);
  if FSearchForm.CaseSensitive then Include(Options, ssoMatchCase);
  if not (FSearchForm.FromCursor or (Action in [atSearchPrev, atSearchNext])) then
    Include(Options, ssoEntireScope);
  if FSearchForm.SelectedOnly then Include(Options, ssoSelectedOnly);
  if FSearchForm.WholeWords then Include(Options, ssoWholeWord);

  if FSynMemo.SearchReplace(Search, Replace, Options) = 0 then
    begin
      if ssoBackwards in Options then
        FSynMemo.BlockEnd := FSynMemo.BlockBegin
      else FSynMemo.BlockBegin := FSynMemo.BlockEnd;
      FSynMemo.CaretXY := FSynMemo.BlockBegin;
      MessageBox(Application.Handle, PAnsiChar('По вашему запросу "' + Search +
        '" ничего не найдено'), 'Ничего не найдено', MB_OK or MB_ICONWARNING);
    end;
end;

procedure TCustomConsole.EnableButtons;
begin
  FAdd.Enabled := True;
  FDel.Enabled := (FComboBox.Text <> '') and (FSynMemo.Text <> '');
  FSave.Enabled := FChanged;
end;

function TCustomConsole.GetEncriptQueries: Boolean;
begin
  Result := FCriptCheck.Checked;
end;

function TCustomConsole.GetFileList: TStrings;
var F: TSearchRec;
begin
  Result := TStringList.Create;
  if SysUtils.FindFirst(FSourcePath + '*' + FFilesExt, faAnyFile and not faDirectory, F) = 0 then
    begin
      repeat
        with TStringList.Create do
          begin
            LoadFromFile(FSourcePath + F.Name);
            if Copy(Text, 1, 3) = 'CR*' then
              Result.Add('* ' + ChangeFileExt(F.Name, ''))
            else Result.Add(ChangeFileExt(F.Name, ''));
            Free;
          end;
      until SysUtils.FindNext(F) <> 0;
      SysUtils.FindClose(F);
    end;
end;

procedure TCustomConsole.HideLogBtnClick(Sender: TObject);
begin
  FResultMemo.Visible := not FResultMemo.Visible;
  if FResultMemo.Visible then
    begin
      if FindResource(hInstance, 'PANEL_HIDE_BTN', RT_BITMAP) <> 0 then
        FLogHide.Glyph.LoadFromResourceName(hInstance, 'PANEL_HIDE_BTN');
      FDownTempPanel.Height := FLogHeight;
    end
  else
    begin
      if FindResource(hInstance, 'PANEL_SHOW_BTN', RT_BITMAP) <> 0 then
        FLogHide.Glyph.LoadFromResourceName(hInstance, 'PANEL_SHOW_BTN');
      FLogHeight := FDownTempPanel.Height;
      FDownTempPanel.Height := FLogHide.Height;
    end;
end;

procedure TCustomConsole.PopupClick(Sender: TObject);
begin
  ChangeColorTheme(TConsoleColorTheme(TComponent(Sender).Tag));
end;

procedure TCustomConsole.Run;
begin
end;

procedure TCustomConsole.SearchReplace(Search, Replace: String;
  Action: TActionType);
begin
  FSearchForm.ActionType := Action;
  FSearchForm.EditSearch.Text := Search;
  FSearchForm.EditReplace.Text := '';
  FSearchForm.Show;
end;

procedure TCustomConsole.SetConsoleColorTheme(const Value: TConsoleColorTheme);
begin
  FConsoleColorTheme := Value;
  ChangeColorTheme(FConsoleColorTheme);
end;

procedure TCustomConsole.SetEncriptQueries(const Value: Boolean);
begin
  FCriptCheck.Checked := Value;
end;

procedure TCustomConsole.SetFilesExt(const Value: String);
begin
  FFilesExt := Value;
  if FFilesExt <> '' then
    if FFilesExt[1] <> '.' then
      FFilesExt := '.' + FFilesExt;
end;

procedure TCustomConsole.SetHighlighter(const Value: TSynCustomHighlighter);
begin
  FHighlighter := Value;
  FSynMemo.Highlighter := FHighlighter;
end;

procedure TCustomConsole.SetSourcePath(const Value: String);
begin
  FSourcePath := IncludeTrailingPathDelimiter(ExpandFileName(Value));
end;

procedure TCustomConsole.SynEditorReplaceText(Sender: TObject; const ASearch,
  AReplace: String; Line, Column: Integer; var Action: TSynReplaceAction);
var
  APos: TPoint;
  EditRect: TRect;
begin
  if ASearch = AReplace then
    Action := raSkip
  else
    begin
      APos := FSynMemo.ClientToScreen(FSynMemo.RowColumnToPixels(
        FSynMemo.BufferToDisplayPos(BufferCoord(Column, Line))));
      EditRect := ClientRect;
      EditRect.TopLeft := ClientToScreen(EditRect.TopLeft);
      EditRect.BottomRight := ClientToScreen(EditRect.BottomRight);

      FConfirmDialog.PrepareShow(EditRect, APos.X, APos.Y,
        APos.Y + FSynMemo.LineHeight, AReplace);
      case FConfirmDialog.ShowModal of
        mrYes: Action := raReplace;
        mrYesToAll: Action := raReplaceAll;
        mrNo: Action := raSkip;
        else Action := raCancel;
      end;
    end;
end;

procedure TCustomConsole.SynKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  CommentString: string;
begin
  case Key of
    VK_F5:
      begin
        CommentString := '';
        if Self is TScriptConsole then
         CommentString := '//';
        FSynMemo.Lines.Insert(0, CommentString + FAuthor + ' ' +  FormatDateTime('dd.mm.yyyy hh:nn', now));
      end;
    VK_F9: ButtonClick(FRun);
    VK_F8: ButtonClick(FDel);
    VK_F2: ButtonClick(FSave);
    Ord('S'):
      if ssCtrl in Shift then
        ButtonClick(FSave);
    Ord('N'):
      if ssCtrl in Shift then
        ButtonClick(FAdd);
    Ord('F'):
      if ssCtrl in Shift then
        ButtonClick(FFind);
    Ord('R'):
      if ssCtrl in Shift then
        ButtonClick(FReplace);
    VK_F3:
      if ssShift in Shift then
        DoSearchReplace(FSearchForm.EditSearch.Text, FSearchForm.EditReplace.Text, atSearchPrev)
      else DoSearchReplace(FSearchForm.EditSearch.Text, FSearchForm.EditReplace.Text, atSearchNext);
  end;
end;

procedure TCustomConsole.SynMemoChange(Sender: TObject);
begin
  FChanged := True;
  EnableButtons;
end;

{ ------------------------------ TScriptCompiler ----------------------------- }

constructor TScriptCompiler.Create(AOwner: TComponent);
begin
  inherited;
  FExportPath := ExpandFileName('.\Files');

  FCompExec := TIFPS3CompExec.Create(self);
  with FCompExec do
    begin
      FreeNotification(Self);
    end;
  FCompExec.OnCompile := Compile;
  FCompExec.OnExecute := Execute;

  FDLLPlugin := TIFPS3DllPlugin.Create(self);
  with FDllPlugin do
    begin
      CompExec := FCompExec;
      FreeNotification(Self);
    end;

  FClassesPlugin := TIFPS3ClassesPlugin.Create(Self);
  with FClassesPlugin do
    begin
      CompExec := FCompExec;
      OnCompImport := CompImport;
      OnExecImport := ExecImport;
      FreeNotification(Self);
    end;

  FScript := TStringList.Create;
  FOutParams := TStringList.Create;
end;

destructor TScriptCompiler.Destroy;
begin
  FScript.Free;
  FOutParams.Free;
  inherited;
end;

procedure TScriptCompiler.DoAferCompImport(x: TIFPSCompileTimeClassesImporter);
begin
  if Assigned(FAfterCompImport) then
    FAfterCompImport(Self, x);
end;

procedure TScriptCompiler.DoAferExecImport(x: TIFPSRuntimeClassImporter);
begin
  if Assigned(FAfterExecImport) then
    FAfterExecImport(Self, x);
end;

{//////////////////////////////////////////////////////////////////////////////}
{ --------------------------- Functions for console -------------------------- }
procedure ShellExecuteOpen(SaveName: string);
begin
  ShellExecute(Application.Handle, 'open', PAnsiChar(SaveName), '', '', SW_SHOW);
end;

function MyReadln(const question: string): string;
begin
  Result := InputBox(question, '', '');
end;

function NowStr(Format: String): String;
begin
  Result := FormatDateTime(Format, Now);
end;

function AppDir: String;
begin
  Result := ExtractFilePath(Application.ExeName);
end;

function FWriteln1(const question: string): string;
var i: integer;
  C: TComponent;
begin
  if csDestroying in Application.ComponentState then Exit;
  for i := 0 to Application.ComponentCount - 1 do
    begin
      C := Application.Components[i].FindComponent('ScriptConsole1');
      if Assigned(C) then
        TScriptConsole(C).FResultMemo.Lines.Add(question);
    end;
end;

function StringReplaceAll(const S, OldPattern, NewPattern: string): string;
begin
  Result := StringReplace(S, OldPattern, NewPattern, [rfReplaceAll, rfIgnoreCase]);
end;

procedure MyOut(const S: String);
var i: integer;
  C: TComponent;
begin
  if csDestroying in Application.ComponentState then Exit;
  for i := 0 to Application.ComponentCount - 1 do
    begin
      C := Application.Components[i].FindComponent('ScriptConsole1');
      if Assigned(C) then
        TScriptConsole(C).ScriptCompiler.OutParams.Add(S);
    end;
end;

function FindFiles(Path, Mask: String; Recursive: Boolean): TStringList;

  procedure ScanDir(Path, Mask: String; var List: TStringList);
  var
    Sr: TSearchRec;
  begin
    Path := IncludeTrailingPathDelimiter(Path);
    if SysUtils.FindFirst(Path + '*.*', faAnyFile, Sr) = 0 then
      begin
        repeat
          if (Sr.Name = '.') or (Sr.Name = '..') then
            Continue
          else
            if Sr.Attr and faDirectory = faDirectory then
              ScanDir(Path + Sr.Name, Mask, List)
            else
              if MatchesMask(Sr.Name, Mask) then
                List.Add(Path + Sr.Name);
        until SysUtils.FindNext(Sr) <> 0;
        SysUtils.FindClose(Sr);
      end;
  end;

var
  Sr: TSearchRec;
begin
  Result := TStringList.Create;
  if not Recursive then
    begin
      Path := IncludeTrailingPathDelimiter(Path);
      if SysUtils.FindFirst(Path + Mask, faAnyFile, Sr) = 0 then
        begin
          repeat
            Result.Add(Path + Sr.Name);
          until SysUtils.FindNext(Sr) <> 0;
          SysUtils.FindClose(Sr);
        end;
    end
  else
    ScanDir(Path, Mask, Result);
end;

{//////////////////////////////////////////////////////////////////////////////}

procedure TScriptCompiler.Compile(Sender: TIFPS3CompExec);
begin
  Sender.AddFunction(@FWriteln1, 'procedure Writeln(s: string);');
  Sender.AddFunction(@MyReadln, 'function Readln(Question: string): string;');
  Sender.AddFunction(@ExtractFilePath, 'function ExtractFilePath(const FileName: string): string;');
  Sender.AddFunction(@Now, 'function Now: TDateTime;');
  Sender.AddFunction(@FormatDateTime, 'function FormatDateTime(const Format: string; DateTime: TDateTime): string;');
  Sender.AddFunction(@NowStr, 'function NowStr(Format: String): String;');
  Sender.AddFunction(@QuotedStr, 'function QuotedStr(const S: string): string;');
  Sender.AddFunction(@AppDir, 'function AppDir: String;');
  Sender.AddFunction(@ForceDirectories, 'function ForceDirectories(Dir: string): Boolean;');
  Sender.AddFunction(@StringReplaceAll, 'function StringReplace(const S, OldPattern, NewPattern: string): string;');
  Sender.AddFunction(@ShellExecuteOpen, 'procedure ShellExecuteOpen(SaveName: string);');
  Sender.AddFunction(@FileExists, 'function FileExists(FileName: string): Boolean;');
  Sender.AddFunction(@ExpandFileName, 'function ExpandFileName(const FileName: string): string;');
  Sender.AddFunction(@Random, 'function Random(const ARange: Integer): Integer;');
  Sender.AddFunction(@Randomize, 'procedure Randomize;');
  Sender.AddFunction(@FindFiles, 'function FindFiles(Path, Mask: String; Recursive: Boolean): TStringList;');

  Sender.AddRegisteredVariable('Application', 'TApplication');
  Sender.AddRegisteredVariable('Self', 'TForm');
  Sender.AddRegisteredVariable('FilesDir', 'String');
end;

procedure TScriptCompiler.CompImport(Sender: TObject;
  x: TIFPSCompileTimeClassesImporter);
begin
  SIRegister_Std(x);
  SIRegister_Classes(x);
  SIRegister_Graphics(x);
  SIRegister_Controls(x);
  SIRegister_stdctrls(x);
  SIRegister_Forms(x);
  SIRegister_word(x);
  SIRegister_dialogs(x);
  SIRegister_sysutils(x);
  SIRegister_excel(x);
  SIRegister_ComCtrls(x);
  SIRegister_ExtCtrls(x);

  DoAferCompImport(x);
end;

procedure TScriptCompiler.ExecImport(Sender: TObject;
  x: TIFPSRuntimeClassImporter);
begin
  RIRegister_Std(x);
  RIRegister_Classes(x);
  RIRegister_Graphics(x);
  RIRegister_Controls(x);
  RIRegister_stdctrls(x);
  RIRegister_Forms(x);
  RIRegister_word(x);
  RIRegister_dialogs(x);
  RIRegister_excel(x);
  RIRegister_ComCtrls(x);
  RIRegister_ExtCtrls(x);

  DoAferExecImport(x);
end;

procedure TScriptCompiler.Execute(Sender: TIFPS3CompExec);
begin
  FClassesPlugin.SetVarToInstance('APPLICATION', Application);
  FClassesPlugin.SetVarToInstance('SELF', Self);
end;

procedure TScriptCompiler.LoadFromFile(FileName: String);
begin
  FScript.LoadFromFile(FileName);
end;

procedure TScriptCompiler.ReplaceVar(OldPattern, NewPattern: string);
begin
  FScript.Text := StringReplace(FScript.Text, OldPattern, NewPattern, [rfReplaceAll]);
end;

procedure TScriptCompiler.Run(Script: String);
begin
  FScript.Text := Script;
  Run(FScript);
  FScript.Free;
end;

procedure TScriptCompiler.Run;
begin
  Run(FScript);
end;

procedure TScriptCompiler.RunFromFile(FileName: String);
begin
  FScript.LoadFromFile(FileName);
  Run(FScript);
end;

procedure TScriptCompiler.Run(Script: TStrings);
var Freq, Time1, Time2: Comp;
  flgTime, WasError: Boolean;
  strTime, LogStr: String;

  function Rounder(Value: Double; Decimals: Integer): Double;
  var j: Integer;
    A: Double;
  begin
    A := 1;
    case Decimals of
      0: A := 1;
      1: A := 10;
      else for j := 1 to Decimals do  A := A * 10;
    end;
    Result := Int((Value * A) + 0.5) / A;
  end;

begin
  if Trim(Script.Text) = '' then Exit;
  FOutParams.Clear;
  WasError := False;
  FCompExec.Script.Assign(Script);
  flgTime := QueryPerformanceFrequency(TLargeInteger((@Freq)^));
  if flgTime then QueryPerformanceCounter(TLargeInteger((@Time1)^));
  // Compile
  if FCompExec.Compile then
    begin
      LogStr := 'Compiled successfully'#13#10;
      // Execute
      if FCompExec.Execute then
        begin  // Execute done
          StrTime := '';
          if flgTime then
            begin
              QueryPerformanceCounter(TLargeInteger((@Time2)^));
              StrTime := (' (' + Sysutils.FloatToStr(Rounder((Time2 - Time1) / Freq, 5)) +' sec)');
            end;
          LogStr := 'Succesfully executed ' + StrTime + #13#10;
        end
      else
        begin // Execute stoped
          LogStr := LogStr + FCompExec.ExecErrorToString +
            ' at '+Inttostr(FCompExec.ExecErrorProcNo)+
            '.'+Inttostr(FCompExec.ExecErrorByteCodePosition);
          WasError := True;
        end;
    end
  else
    begin
      LogStr := 'Compiling failed';
      WasError := True;
    end;
  if WasError then
    MessageBox(Application.Handle, PAnsiChar('При исполнении возникла ошибка: '#13#10 +
      LogStr), 'Ошибка', MB_OK or MB_ICONERROR);
end;

procedure TScriptCompiler.SetCompExec(const Value: TIFPS3CompExec);
begin
  FCompExec := Value;
end;

procedure TScriptCompiler.SetExportPath(const Value: String);
begin
  FExportPath := Value;
end;

procedure TScriptCompiler.SetScript(const Value: TStrings);
begin
  FScript := Value;
end;

{ ------------------------------ TScriptConsole ------------------------------ }

procedure TScriptConsole.ChangeColorTheme(
  AConsoleColorTheme: TConsoleColorTheme);
begin
  inherited;
  if not Assigned(SynHighLighter) then Exit;
  case AConsoleColorTheme of
    cctLight:
      with TSynPASSyn(SynHighLighter) do
        begin
          AsmAttri.Foreground := clNone;
          AsmAttri.Style := [];
          CommentAttri.Foreground := clGreen;
          CommentAttri.Style := [fsItalic];
          DirectiveAttri.Foreground := clTeal;
          DirectiveAttri.Style := [];
          FloatAttri.Foreground := clBlue;
          FloatAttri.Style := [];
          KeyAttri.Foreground := clNavy;
          KeyAttri.Style := [fsBold];
          NumberAttri.Foreground := clBlue;
          NumberAttri.Style := [];
          HexAttri.Foreground := clBlue;
          HexAttri.Style := [];
          StringAttri.Foreground := clBlue;
          StringAttri.Style := [];
          CharAttri.Foreground := clBlue;
          CharAttri.Style := [];
          SymbolAttri.Foreground := clNone;
          SymbolAttri.Style := [];
        end;
    cctDark:
      with TSynPASSyn(SynHighLighter) do
        begin
          AsmAttri.Foreground := clLime;
          AsmAttri.Style := [];
          CommentAttri.Foreground := clSilver;
          CommentAttri.Style := [fsItalic];
          DirectiveAttri.Foreground := clSilver;
          DirectiveAttri.Style := [];
          FloatAttri.Foreground := clFuchsia;
          FloatAttri.Style := [fsBold];
          KeyAttri.Foreground := clWhite;
          KeyAttri.Style := [];
          NumberAttri.Foreground := clFuchsia;
          NumberAttri.Style := [fsBold];
          HexAttri.Foreground := clFuchsia;
          HexAttri.Style := [fsBold];
          StringAttri.Foreground := clAqua;
          StringAttri.Style := [];
          CharAttri.Foreground := clAqua;
          CharAttri.Style := [];
          SymbolAttri.Foreground := clLime;
          SymbolAttri.Style := [];
        end;
  end;
end;

constructor TScriptConsole.Create(AOwner: TComponent);
begin
  inherited;
  SynHighLighter := TSynPASSyn.Create(Self);
  with TSynPASSyn(SynHighLighter) do
    begin
      FreeNotification(Self);
    end;
  ChangeColorTheme(cctLight);
end;

procedure TScriptConsole.Run;
var flgTime: Boolean;
  Freq, Time1, Time2: Comp;
  StrTime: String;

  procedure OutputMessages;
  var l, Selection: Longint;
    b: Boolean;
  begin
    b := False;
    for l := 0 to FScriptCompiler.CompExec.CompilerMessageCount - 1 do
      begin
        FSynMemo.SelStart := FScriptCompiler.CompExec.CompilerMessages[l].Position;
        FResultMemo.Lines.Add('Compiler: ' + FScriptCompiler.CompExec.CompilerErrorToStr(l) + ' >> at line [ ' +
          IntToStr(FSynMemo.CaretY) + ' ]');
        if (not b) and (FScriptCompiler.CompExec.CompilerMessages[l].MessageType in [ptWarning, ptError]) then
          begin
            b := True;
            Selection := FScriptCompiler.CompExec.CompilerMessages[l].Position;
          end;
      end;
    if b then
      FSynMemo.SelStart := Selection;
  end;

  procedure OutputExecuteMessages;
  var
    Err: String;
  begin
    FSynMemo.SelStart := FScriptCompiler.CompExec.ExecErrorPosition;

    FSynMemo.SelStart := FScriptCompiler.CompExec.ExecErrorPosition;
    Err := FScriptCompiler.CompExec.ExecErrorToString + ' at ' +
      FScriptCompiler.CompExec.Exec.ProcNames.GetItem(FScriptCompiler.CompExec.ExecErrorProcNo) +
      ' : ' + Inttostr(FScriptCompiler.CompExec.ExecErrorPosition) + ' on line ' +
      IntToStr(FSynMemo.CaretY);
    FResultMemo.Lines.Add('Executor: ' + Err);
  end;

  function Rounder(Value: Double; Decimals: Integer): Double;
  var j: Integer;
    A: Double;
  begin
    A := 1;
    case Decimals of
      0: A := 1;
      1: A := 10;
      else for j := 1 to Decimals do A := A * 10;
    end;
    Result := Int((Value * A) + 0.5) / A;
  end;

begin
  inherited;
  if Trim(FSynMemo.Text) = '' then Exit;
  FScriptCompiler.CompExec.Script.Assign(FSynMemo.Lines);
  FResultMemo.Lines.Clear;
  flgTime := QueryPerformanceFrequency(TLargeInteger((@Freq)^));
  if flgTime then QueryPerformanceCounter(TLargeInteger((@Time1)^));
  // Compile
  if FScriptCompiler.CompExec.Compile then
    begin
      OutputMessages;
      FResultMemo.Lines.Add('Compiled succesfully');
      // Execute
      if FScriptCompiler.CompExec.Execute then
        begin // Execute done
          StrTime := '';
          if flgTime then
            begin
              QueryPerformanceCounter(TLargeInteger((@Time2)^));
              StrTime := ' (' + FloatToStr(Rounder((Time2 - Time1) / Freq, 5)) + ' sec)';
            end;
          FResultMemo.Lines.Add('Succesfully executed ' + StrTime);
        end
      else
        begin // Execute error
          OutputExecuteMessages;
        end;
    end
  else
    begin
      OutputMessages;
      FResultMemo.Lines.Add('Compiling failed');
    end;
end;

procedure TScriptConsole.SetScriptCompiler(const Value: TScriptCompiler);
begin
  FScriptCompiler := Value;
end;

end.
