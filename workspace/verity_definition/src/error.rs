pub struct ErrorLine {
    pub left: usize,
    pub right: usize,
    pub error_left: usize,
    pub error_right: usize,
}

pub struct Error {
    pub lineno: Option<usize>,
    pub colno: Option<usize>,
    pub message: Option<String>,
    pub error_line: Option<ErrorLine>,
}

impl Error {
    pub fn at(contents: &str, location: usize, message: &str) -> Self {
        let ((line_head, line_end), (lineno, colno)) = position(&contents, location);

        let line = &contents[line_head..line_end];
        let line_end = line_head + line.trim_end().len();

        let message = String::from(message);

        let error_line = ErrorLine {
            left: line_head,
            error_left: location,
            error_right: line_end,
            right: line_end,
        };

        Error {
            lineno: Some(lineno),
            colno: Some(colno),
            message: Some(message),
            error_line: Some(error_line),
        }
    }

    pub fn between(contents: &str, location: (usize, usize), message: &str) -> Self {
        let (left, right) = location;
        let ((line_head, _line_end), (lineno, colno)) = position(&contents, left);
        let ((_line_head, line_end), (_lineno, _colno)) = position(&contents, right);

        let line = &contents[line_head..line_end];
        let line_end = line_head + line.trim_end().len();

        let message = String::from(message);

        let error_line = ErrorLine {
            left: line_head,
            error_left: left,
            error_right: right,
            right: line_end,
        };

        Error {
            lineno: Some(lineno),
            colno: Some(colno),
            message: Some(message),
            error_line: Some(error_line),
        }
    }
}

fn position(contents: &str, location: usize) -> ((usize, usize), (usize, usize)) {
    let mut line_head: usize = 0;
    let mut line_end: usize = 0;
    let mut lineno: usize = 0;
    let mut colno: usize = 0;

    for (i, c) in contents.chars().enumerate() {
        if i < location && c == '\n' {
            line_head = i + 1;
        }

        line_end = i + 1;

        if i < location {
            if c == '\n' {
                lineno += 1;
                colno = 0;
            } else {
                colno += 1;
            }
        }

        if i >= location && c == '\n' {
            break;
        }
    }

    ((line_head, line_end), (lineno, colno))
}

pub fn eprintln_error(subject: &str, lineno: usize, colno: usize, contents: &str, error: Error) {
    let lineno = if let Some(i) = error.lineno {
        lineno + i
    } else {
        lineno
    };

    let colno = if let Some(j) = error.colno {
        colno + j
    } else {
        colno
    };

    if let Some(message) = error.message {
        eprintln!("\x1B[1m{subject}:{lineno}:{colno}:\x1B[0m \x1B[1;31merror:\x1B[0m \x1B[1m{message}\x1B[0m");
    }

    if let Some(error_line) = error.error_line {
        let prefix = &contents[error_line.left..error_line.error_left];
        let interfix = &contents[error_line.error_left..error_line.error_right];
        let suffix = &contents[error_line.error_right..error_line.right].trim_end();

        let interfix = interfix
            .lines()
            .map(|fragment| format!("\x1B[7;31m{fragment}\x1B[0m"))
            .collect::<Vec<_>>()
            .join("\n");

        eprintln!("{prefix}{interfix}{suffix}");
    }

    eprintln!("");
}
