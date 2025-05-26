// Главное окно программы
unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, Menus,
  ActnList, StdActns, ExtCtrls, DateTimePicker,

  DateUtils,
  Math,  // sign
  DBRecord, ReportView;

type

  { TFormMain }

  TFormMain = class(TForm)
    // "Список действий"
    ActionList: TActionList;

    ActionSaveRecord: TAction;
    ActionEdit: TAction;
    ActionReport: TAction;
    FileOpen: TFileOpen;
    FileSaveAs: TFileSaveAs;

    EditTitle: TEdit;
    EditAuthor: TEdit;
    EditCard: TEdit;
    EditReader: TEdit;
    DateReturn: TDateTimePicker;

    ButtonAdd: TButton;
    ButtonDelete: TButton;
    ButtonToggleEdit: TButton;
    ButtonDup: TButton;
    ButtonSearch: TButton;

    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuFileOpen: TMenuItem;
    MenuFileSaveAs: TMenuItem;
    MenuService: TMenuItem;
    MenuServiceReport: TMenuItem;

    PanelRecord: TPanel;
    PanelButtons: TPanel;
    StringGrid: TStringGrid;

    procedure ActionReportExecute(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonDeleteClick(Sender: TObject);
    procedure ButtonToggleEditClick(Sender: TObject);
    procedure ButtonDupClick(Sender: TObject);
    procedure ButtonEditSaveClick(Sender: TObject);
    procedure FileOpenAccept(Sender: TObject);
    procedure FileSaveAsAccept(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StringGridCompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; var Result: integer);
    procedure StringGridResize(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);

  private

  public

  end;

var
  FormMain: TFormMain;

implementation

type
  // Колонки таблицы.
  // NOTE: отличаются от TStoredFields
  TDBColumn = (colID = 0, colTitle, colAuthor, colCard, colReader, colDate);

var
  // Указатель на редактируемый элемент списка
  // nil, если не в режиме редактирования
  PEditedRecord: PItem;
  // Общий список записей базы данных
  storage: TRecordList;
  // Фильтрованый список базы данных
  // пуст, если фильтры не активны
  filtered: TRecordList;


{$R *.lfm}

{ TFormMain }

// Создание экземпляра формы. Инициализация переменных
procedure TFormMain.FormCreate(Sender: TObject);
begin
  storage := TRecordList.Create;
  filtered := TRecordList.Create;
  PEditedRecord := nil;

  DateReturn.Date := Today;
end; 

// Удаление экземпляра формы. Освобождение памяти
procedure TFormMain.FormDestroy(Sender: TObject);
begin
  storage.free;
  filtered.free;
end;

// FIXME: не сразу разворачивается на весь экран
// Обработчик события показа формы
procedure TFormMain.FormShow(Sender: TObject);
begin
  //AutoSize := false;
  //AutoSize := true;
  //AutoSize := false;
end;

// Сравнить две записи по определенному столбцу.
function CompareByCol(a, b: TRecord; key: TDBColumn): Integer;
const
  // Получить поле записи по столбцу.
  ColToField: array[TDBColumn] of TStoredFields = (TStoredFields(0), fTitle, fAuthor, fCard, fReader, fDate);
begin
  if key = colID then
  begin
    // Сравнить по ID
    result := sign(a.recordID - b.recordID);
    exit;
  end;

  result := CompareBy(a, b, ColToField[key]);
end;

// Сравнить ячейки таблицы
procedure TFormMain.StringGridCompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; var Result: integer);
var
  aID, bID: integer;
begin
  result := 0;

  assert(ACol = BCol);

  aID := StringGrid.rows[ARow][Ord(colID)].toInteger;
  bID := StringGrid.rows[BRow][Ord(colID)].toInteger;

  result := CompareByCol(ItemByID(storage, aID)^.Data,
                         ItemByID(storage, bID)^.Data,
                         TDBColumn(ACol));

  // Результаты сортировки автоматически не обращаются
  // по возрастанию/убыванию
  if StringGrid.SortOrder = soDescending then
    result := -result;
end;

// Автоматитески изменить размер при изменении размера таблицы
procedure TFormMain.StringGridResize(Sender: TObject);
begin
  StringGrid.AutoFillColumns := true;
  StringGrid.AutoFillColumns := false;
end;

// Отобразить список в таблице
procedure Redisplay(List: TRecordList);
var
  rec: TRecord;
  i: integer;
  OldRow: integer;
begin
  // сохранить номер выделенной строки
  OldRow := FormMain.StringGrid.Row;
                                        
  //FormMain.stringgrid1.Clear;
  //FormMain.StringGrid1.Clean([gzNormal]);
  // Начать с единственной строки - заголовка.
  FormMain.StringGrid.RowCount := 1;
  i := 1;
  for rec in List do
  begin
    with FormMain.StringGrid do
    begin
      RowCount := RowCount + 1;

      Rows[i][ord(colID)] := rec.recordID.toString;
      Rows[i][ord(colTitle)] := rec.name;
      Rows[i][ord(colAuthor)] := rec.author;
      Rows[i][ord(colCard)] := rec.cardID;
      Rows[i][ord(colReader)] := rec.reader;
      Rows[i][ord(colDate)] := DateToStr(rec.returndate);
    end;

    inc(i);
  end;

  // вернуть выделение
  FormMain.StringGrid.Row := OldRow;
end;

// Найти выделенную строку таблицы (по колонке ID)
function SelectedRecord: TRecordList.PItem;
var
  i: integer;
  cur: PItem;
begin
  // Ищем по колонке ID
  with FormMain.StringGrid do
    i := Rows[Row][Ord(colID)].toInteger;
  cur := storage.First;
  while cur <> nil do
  begin
    if cur^.Data.recordID = i then
    begin
      result := cur;
      exit;
    end;
    cur := cur^.Next;
  end;
end;

// Запись, заполненная полями ввода
function RecordFromInput: TRecord;
begin
  result.recordID := 0;
  result.name := FormMain.EditTitle.Text;
  result.author := FormMain.EditAuthor.Text;
  result.cardID := FormMain.EditCard.Text;
  result.reader := FormMain.EditReader.Text;
  result.returndate := FormMain.DateReturn.date;
end;

// Очистить поля ввода
procedure ClearInput;
begin
  FormMain.EditTitle.Clear;
  FormMain.EditAuthor.Clear;
  FormMain.EditCard.Clear;
  FormMain.EditReader.Clear;
  FormMain.DateReturn.date := today;
end;

// Кнопка добавить
procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  if PEditedRecord <> nil then  // режим редактирования
    ButtonEditSaveClick(self)
  else
    storage.InsertLast( RecordFromInput );
  RenumberList(storage);  // NOTE: номера записей меняются
                          //       в соответствии с их порядком в списке
  Redisplay(storage);
  ClearInput;
end;

// Показать отчет
procedure TFormMain.ActionReportExecute(Sender: TObject);
begin
  FormReport.show;
  ShowReport(storage);
end;

// Кнопка удалить
procedure TFormMain.ButtonDeleteClick(Sender: TObject);
begin
  // Защита от ошибки при пустом списке
  if storage.Count = 0 then exit;

  storage.Delete( SelectedRecord );
  RenumberList(storage);
  Redisplay(storage);
end;

// Обновить поля при входе в режим редактирования
procedure EnterEditMode;
begin
  FormMain.ButtonAdd.Caption := 'Сохранить';
  FormMain.ButtonToggleEdit.Caption := 'Отмена';
  FormMain.ButtonDelete.Enabled := false;
  FormMain.ButtonDup.Enabled := false;
  FormMain.ButtonSearch.Enabled := false;
end;

// Обновить поля при выходе из режима редактирования
procedure ExitEditMode;
begin
  PEditedRecord := nil;
  FormMain.ButtonAdd.Caption := 'Добавить';
  FormMain.ButtonToggleEdit.Caption := 'Редактировать';
  FormMain.ButtonDelete.Enabled := true;
  FormMain.ButtonDup.Enabled := true;
  FormMain.ButtonSearch.Enabled := true;
end;


// Переключить режим редактирования
procedure TFormMain.ButtonToggleEditClick(Sender: TObject);
begin
  // Защита от ошибки при пустом списке
  if storage.Count = 0 then exit;

  // Режим редактирования активен, отменить
  if PEditedRecord <> nil then
  begin
    ExitEditMode;
    ClearInput;
    exit;
  end;

  // Выбрать запись для редактирования
  PEditedRecord := SelectedRecord;

  // Вставить в поля ввода
  EditTitle.Text := PEditedRecord^.data.name;
  EditAuthor.Text := PEditedRecord^.data.author;
  EditCard.Text := PEditedRecord^.data.CardID;
  EditReader.Text := PEditedRecord^.data.Reader;
  DateReturn.date := PEditedRecord^.data.returndate;

  EnterEditMode;
end;

// Кнопка дублировать
procedure TFormMain.ButtonDupClick(Sender: TObject);
begin
  // Защита от ошибки при пустом списке
  if storage.Count = 0 then exit;

  SelectedRecord^.InsertAfter(SelectedRecord^.data);
  RenumberList(storage);
  Redisplay(storage);
  // Выделить новую запись
  StringGrid.Row := StringGrid.row + 1;
end;

// Сохранить изменения записи
procedure TFormMain.ButtonEditSaveClick(Sender: TObject);
begin
  // Если не в режиме редактирования
  if PEditedRecord = nil then exit;

  PEditedRecord^.data := RecordFromInput;
  RenumberList(storage);
  Redisplay(storage);
  ExitEditMode;
end;

// Соответствует ли запись критериям поиска?
function FilterRec(rec: TRecord): Boolean;
var
  Filter: TRecord;
begin
  Filter := RecordFromInput;
  result := true;

  if Filter.name <> '' then
    if Pos(filter.name, rec.name) = 0 then
      result := false;

  if Filter.author <> '' then
    if Pos(filter.author, rec.author) = 0 then
      result := false;

  if Filter.reader <> '' then
    if Pos(filter.reader, rec.reader) = 0 then
      result := false;

  if Filter.cardID <> '' then
    if Pos(filter.cardID, rec.cardID) = 0 then
      result := false;

  // Date?
end;

// Кнопка поиска (фильтр)
procedure TFormMain.ButtonSearchClick(Sender: TObject);
var
  rec: TRecord;
begin
  filtered.Clear;
  // Заполнить filtered
  for rec in storage do
    if FilterRec(rec) then
      filtered.InsertLast(rec);
  // NOTE: Не менять колонку ID! Это сломает поиск записей
  redisplay(filtered);
end;

// Принятие файла на чтение
procedure TFormMain.FileOpenAccept(Sender: TObject);
begin
  storage.Clear;
  try
  with FileOpen.Dialog do
    if FileName.EndsWith('.TXT', true) or FileName.EndsWith('.CSV', true) then
    begin
      storage.free;  // CSVToList создает новый список
      storage := CSVToList(FileName);
    end
    else
      LoadFromBinaryFile(storage, filename);
  redisplay(storage);
  except on e: Exception do
    ShowMessage(e.Message);
  end;
end;

// Принятие файла на запись
procedure TFormMain.FileSaveAsAccept(Sender: TObject);
begin
  with FileSaveAs.Dialog do
    if FileName.EndsWith('.TXT', true) or FileName.EndsWith('.CSV', true) then
      ListToCSV(storage, FileName)
    else
      SaveToBinaryFile(storage, filename);
end;

end.

