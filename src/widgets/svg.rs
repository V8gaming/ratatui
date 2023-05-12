use std::borrow::Cow;

use crate::{style::{Style, Color}, layout::{Rect, Alignment}, buffer::Buffer, text::{Spans, Span}, symbols};

use super::{Block, Widget, canvas::{Canvas, Points, Line}, GraphType};
/// An X or Y axis for the chart widget
#[derive(Debug, Clone)]
pub struct SVGAxis {
    /// Bounds for the axis (all data points outside these limits will not be represented)
    bounds: [f64; 2],
    /// The style used to draw the axis itself
    style: Style,
}

impl Default for SVGAxis {
    fn default() -> SVGAxis {
        SVGAxis {
            bounds: [0.0, 0.0],
            style: Default::default(),
        }
    }
}

impl SVGAxis {
    pub fn bounds(mut self, bounds: [f64; 2]) -> SVGAxis {
        self.bounds = bounds;
        self
    }

    pub fn style(mut self, style: Style) -> SVGAxis {
        self.style = style;
        self
    }

}


/// A group of data points
#[derive(Debug, Clone)]
pub struct SVGDataset<'a> {
    /// Name of the dataset (used in the legend if shown)
    name: Cow<'a, str>,
    /// A reference to the actual data
    data: &'a [(f64, f64, bool)],
    /// Symbol used for each points of this dataset
    marker: symbols::Marker,
    /// Determines graph type used for drawing points
    graph_type: GraphType,
    /// Style used to plot this dataset
    style: Style,
}

impl<'a> Default for SVGDataset<'a> {
    fn default() -> SVGDataset<'a> {
        SVGDataset {
            name: Cow::from(""),
            data: &[],
            marker: symbols::Marker::Dot,
            graph_type: GraphType::Scatter,
            style: Style::default(),
        }
    }
}

impl<'a> SVGDataset<'a> {
    pub fn name<S>(mut self, name: S) -> SVGDataset<'a>
    where
        S: Into<Cow<'a, str>>,
    {
        self.name = name.into();
        self
    }

    pub fn data(mut self, data: &'a [(f64, f64, bool)]) -> SVGDataset<'a> {
        self.data = data;
        self
    }

    pub fn marker(mut self, marker: symbols::Marker) -> SVGDataset<'a> {
        self.marker = marker;
        self
    }

    pub fn graph_type(mut self, graph_type: GraphType) -> SVGDataset<'a> {
        self.graph_type = graph_type;
        self
    }

    pub fn style(mut self, style: Style) -> SVGDataset<'a> {
        self.style = style;
        self
    }
}

#[derive(Debug, Clone)]
pub struct SVG<'a> {
    /// A block to display around the widget eventually
    block: Option<Block<'a>>,
    /// The horizontal axis
    x_axis: SVGAxis,
    /// The vertical axis
    y_axis: SVGAxis,
    /// A reference to the datasets
    datasets: Vec<SVGDataset<'a>>,
    /// The widget base style
    style: Style,
}
#[derive(Debug, Clone, PartialEq, Default)]
struct ChartLayout {
    /// Area of the graph
    graph_area: Rect,
}


impl<'a> SVG<'a> {
    pub fn new(datasets: Vec<SVGDataset<'a>>) -> SVG<'a> {
        SVG {
            block: None,
            x_axis: SVGAxis::default(),
            y_axis: SVGAxis::default(),
            style: Default::default(),
            datasets,
        }
    }

    pub fn block(mut self, block: Block<'a>) -> SVG<'a> {
        self.block = Some(block);
        self
    }

    pub fn style(mut self, style: Style) -> SVG<'a> {
        self.style = style;
        self
    }

    pub fn x_axis(mut self, size: i32) -> SVG<'a> {
        self.x_axis = SVGAxis::default().bounds([0.0, size as f64]);
        self
    }

    pub fn y_axis(mut self, size: i32) -> SVG<'a> {
        self.y_axis = SVGAxis::default().bounds([0.0, size as f64]);
        self
    }
    fn layout(&self, area: Rect) -> ChartLayout {
        let mut layout = ChartLayout::default();
        if area.height == 0 || area.width == 0 {
            return layout;
        }
        let x = area.left();
        let y = area.bottom() - 1;

        if x < area.right() && y > 1 {
            layout.graph_area = Rect::new(x, area.top(), area.right() - x, y - area.top() + 1);
        }

        layout
    }

}
impl<'a> Widget for SVG<'a> { 
    fn render(mut self, area: Rect, buf: &mut Buffer) {
        if area.area() == 0 {
            return;
        }
        buf.set_style(area, self.style);
        // Sample the style of the entire widget. This sample will be used to reset the style of
        // the cells that are part of the components put on top of the grah area (i.e legend and
        // axis names).
        let original_style = buf.get(area.left(), area.top()).style();

        let chart_area = match self.block.take() {
            Some(b) => {
                let inner_area = b.inner(area);
                b.render(area, buf);
                inner_area
            }
            None => area,
        };

        let layout = self.layout(chart_area);
        let graph_area = layout.graph_area;
        if graph_area.width < 1 || graph_area.height < 1 {
            return;
        }

        for dataset in &self.datasets {
            Canvas::default()
                .background_color(self.style.bg.unwrap_or(Color::Reset))
                .x_bounds(self.x_axis.bounds)
                .y_bounds(self.y_axis.bounds)
                .marker(dataset.marker)
                .paint(|ctx| {
                    ctx.draw(&Points {
                        coords: dataset.data,
                        color: dataset.style.fg.unwrap_or(Color::Reset),
                    });
                    if let GraphType::Line = dataset.graph_type {
                        for data in dataset.data.windows(2) {
                            if !data[1].2 {
                                continue;
                            } else {
                                ctx.draw(&Line {
                                    x1: data[0].0,
                                    y1: data[0].1,
                                    x2: data[1].0,
                                    y2: data[1].1,
                                    color: dataset.style.fg.unwrap_or(Color::Reset),
                                })
                            }
                        }
                    }
                })
                .render(graph_area, buf);
        }

    }
    
}
