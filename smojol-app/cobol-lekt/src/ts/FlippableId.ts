export type MutableCenter = {id: string, flip: boolean};

export function randomColour() {
    const hue = Math.floor(Math.random() * 360);
    const saturation = 100;
    const lightness = Math.floor(Math.random() * 50 + 25); // Lightness between 25 and 75 for vibrancy

    return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
}
