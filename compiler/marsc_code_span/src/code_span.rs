#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeSpan {
    start: usize,
    end: usize,
    literal: String,
}

impl CodeSpan {
    pub fn new(start: usize, end: usize, literal: String) -> Self {
        Self {
            start,
            end,
            literal,
        }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn end(&self) -> usize {
        self.end
    }

    pub fn combine(mut spans: Vec<Self>) -> Self {
        if spans.is_empty() {
            panic!("Cannot combine empty spans")
        }
        spans.sort_by(|a, b| a.start.cmp(&b.start));

        let start = spans.first().unwrap().start;
        let end = spans.last().unwrap().end;

        CodeSpan::new(
            start,
            end,
            spans.into_iter().map(|span| span.literal).collect(),
        )
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }

    pub fn literal<'a>(&self, input: &'a str) -> &'a str {
        &input[self.start..self.end]
    }
}
